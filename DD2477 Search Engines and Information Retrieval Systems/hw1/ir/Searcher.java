/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, 2017
 */

package ir;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Searches an index for results of a query.
 */
public class Searcher {

    /** The index to be searched by this Searcher. */
    Index index;

    /** The k-gram index to be searched by this Searcher */
    KGramIndex kgIndex;

    /** Constructor */
    public Searcher(Index index, KGramIndex kgIndex) {
        this.index = index;
        this.kgIndex = kgIndex;
    }

    int noSkips = 0;
    int skipDistance = 0;

    /**
     * Searches the index for postings matching the query.
     * 
     * @return A postings list representing the result of the query.
     */
    public PostingsList search(Query query, QueryType queryType, RankingType rankingType, NormalizationType normType) {

        noSkips = 0;
        skipDistance = 0;

        switch (queryType) {
            case INTERSECTION_QUERY:
                return intersectionQuery(query);
            case PHRASE_QUERY:
                return phraseQuery(query);
            case RANKED_QUERY:
                return rankedQuery(query);
            default:
                break;
        }

        return null;
    }

    private PostingsList intersectionQuery(Query query) {
        var queryPostingsList = new ArrayList<PostingsList>();

        // step 1: collect all posting list for individual terms
        var queryTermSet = new HashSet<String>();
        for (var queryTerm : query.queryterm) {
            queryTermSet.add(queryTerm.term);
        }

        for (var term : queryTermSet) {
            queryPostingsList.add(index.getPostings(term));
        }

        if (queryPostingsList.size() == 0) {
            return null;
        }

        // step 2: intersection algorithm with the rest
        var totalAnswer = new PostingsList(queryPostingsList.get(0));

        for (int queryListIndex = 1; queryListIndex < queryPostingsList.size(); queryListIndex++) {
            var queryList = queryPostingsList.get(queryListIndex);

            totalAnswer = intersectComparison(totalAnswer, queryList);

            if (totalAnswer.size() == 0) {
                break;
            }
        }

        return totalAnswer;
    }

    private PostingsList intersectComparison(PostingsList p1, PostingsList p2) {
        var answer = makeIntersectionQuery(p1, p2, (entry1, entry2) -> {
            return entry1;
        });

        return answer;
    }

    private PostingsList phraseQuery(Query query) {
        var cache = new HashMap<String, PostingsList>();

        if (query.size() == 0) {
            return null;
        }

        var intialPostingsList = index.getPostings(query.queryterm.get(0).term);
        var totalAnswer = new PostingsList(intialPostingsList);

        var start = System.currentTimeMillis();

        for (int i = 0; i < query.size(); i++) {
            var token = query.queryterm.get(i).term;

            var cachedList = cache.get(token);
            if (cachedList == null) {
                cachedList = index.getPostings(token);
                cache.put(token, cachedList);
            }

            totalAnswer = phraseComparison(totalAnswer, cachedList);

            if (totalAnswer.size() == 0) {
                break;
            }
        }

        System.out.println("skips: " + noSkips);
        System.out.println("skip distance: " + skipDistance);

        var end = System.currentTimeMillis();

        System.out.println("phrase query took: " + (end - start) + " ms");

        return totalAnswer;
    }

    private PostingsList phraseComparison(PostingsList p1, PostingsList p2) {
        var answer = makeIntersectionQuery(p1, p2, (entry1, entry2) -> {
            var combined = new PostingsEntry(entry1.docID, 0);

            var entry1SkipInterval = (int) Math.sqrt(entry1.offsets.size());
            var entry2SkipInterval = (int) Math.sqrt(entry2.offsets.size());

            for (int e1 = 0; e1 < entry1.offsets.size(); e1++) {
                for (int e2 = 0; e2 < entry2.offsets.size(); e2++) {
                    if (canSkipOffsets(e1, entry1SkipInterval, entry2.offsets.get(e2), entry1.offsets)) {
                        e1 += entry1SkipInterval;

                        noSkips++;
                        skipDistance += entry1SkipInterval;
                    }

                    if (canSkipOffsets(e2, entry2SkipInterval, entry1.offsets.get(e1), entry2.offsets)) {
                        e2 += entry2SkipInterval;

                        noSkips++;
                        skipDistance += entry2SkipInterval;
                    }

                    var offset1 = entry1.offsets.get(e1);
                    var offset2 = entry2.offsets.get(e2);

                    if (offset2 == offset1 + 1) {
                        combined.offsets.add(offset2);
                    }
                }
            }

            if (combined.offsets.size() != 0) {
                return combined;
            }

            return null;
        });

        return answer;
    }

    private interface IntersectHandler {
        public PostingsEntry onMatch(PostingsEntry entry1, PostingsEntry entry2);
    }

    private PostingsList makeIntersectionQuery(PostingsList p1, PostingsList p2, IntersectHandler handler) {
        var answer = new PostingsList(p2.getToken());

        var p1SkipInterval = (int) Math.sqrt(p1.size());
        var p2SkipInterval = (int) Math.sqrt(p2.size());

        int i = 0, j = 0;
        while (i < p1.size() && j < p2.size()) {
            var entry1 = p1.get(i);
            var entry2 = p2.get(j);

            if (entry1.docID == entry2.docID) {

                var result = handler.onMatch(entry1, entry2);
                if (result != null) {
                    answer.add(result);
                }

                i++;
                j++;
            } else if (entry1.docID < entry2.docID) {
                if (canSkipDocuments(i, p1SkipInterval, entry1.docID, p1)) {
                    i += p1SkipInterval;
                } else {
                    i++;
                }
            } else {
                if (canSkipDocuments(j, p2SkipInterval, entry1.docID, p2)) {
                    j += p2SkipInterval;
                } else {
                    j++;
                }
            }
        }

        return answer;
    }

    private boolean canSkipOffsets(int index, int interval, int otherOffset, List<Integer> offsets) {
        if (index % interval != 0) {
            return false;
        }

        var skipTo = index + interval;
        if (skipTo >= offsets.size()) {
            return false;
        }

        var offset1Skip = offsets.get(skipTo);
        if (offset1Skip >= otherOffset) {
            return false;
        }

        return true;
    }

    private boolean canSkipDocuments(int index, int interval, int otherDocId, PostingsList postingsList) {
        if (index % interval != 0) {
            return false;
        }

        var skipTo = index + interval;
        if (skipTo >= postingsList.size()) {
            return false;
        }

        var entry2Skip = postingsList.get(skipTo);
        if (entry2Skip.docID > otherDocId) {
            return false;
        }

        return true;
    }

    private PostingsList rankedQuery(Query query) {
        return null;
    }
}