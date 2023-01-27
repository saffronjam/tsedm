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

    /**
     * Searches the index for postings matching the query.
     * 
     * @return A postings list representing the result of the query.
     */
    public PostingsList search(Query query, QueryType queryType, RankingType rankingType, NormalizationType normType) {

        System.out.println("query term: " + Arrays.toString(query.queryterm.toArray()));

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

            int i = 0, j = 0;

            var answer = new PostingsList(queryList.getToken());
            while (i < totalAnswer.size() && j < queryList.size()) {
                var entry1 = totalAnswer.get(i);
                var entry2 = queryList.get(j);

                if (entry1.docID == entry2.docID) {
                    answer.add(entry1);
                    i++;
                    j++;
                } else if (entry1.docID < entry2.docID) {
                    i++;
                } else {
                    j++;
                }
            }
            totalAnswer = answer;

            if (totalAnswer.size() == 0) {
                break;
            }
        }

        return totalAnswer;
    }

    private PostingsList phraseQuery(Query query) {
        var queryPostingsMap = new HashMap<String, PostingsList>();
        var queryPostingsList = new ArrayList<PostingsList>();

        // step 1: collect all posting list for individual terms
        for (var queryTerm : query.queryterm) {
            var term = queryTerm.term;

            if (queryPostingsMap.containsKey(term)) {
                queryPostingsList.add(queryPostingsMap.get(term));
            } else {
                var newList = index.getPostings(term);
                queryPostingsMap.put(term, newList);
                queryPostingsList.add(newList);
            }
        }

        if (queryPostingsList.size() == 0) {
            return null;
        }

        // step 2: modified intersection algorithm with the rest
        var totalAnswer = new PostingsList(queryPostingsList.get(0));

        var start = System.currentTimeMillis();

        int noSkips = 0;
        int skipDistance = 0;
        
        for (int queryListIndex = 1; queryListIndex < queryPostingsList.size(); queryListIndex++) {
            var queryList = queryPostingsList.get(queryListIndex);

            var totalAnswerSkipInterval = (int) Math.sqrt(totalAnswer.size());
            var queryListSkipInterval = (int) Math.sqrt(queryList.size());

            int i = 0, j = 0;


            var answer = new PostingsList(queryList.getToken());
            while (i < totalAnswer.size() && j < queryList.size()) {
                var entry1 = totalAnswer.get(i);
                var entry2 = queryList.get(j);
                var combined = new PostingsEntry(entry1.docID, 0);

                var entry1SkipInterval = (int) Math.sqrt(entry1.offsets.size());
                var entry2SkipInterval = (int) Math.sqrt(entry2.offsets.size());

                if (entry1.docID == entry2.docID) {

                    for (int e1 = 0; e1 < entry1.offsets.size(); e1++) {
                        for (int e2 = 0; e2 < entry2.offsets.size(); e2++) {
                            // check if we can use some skips
                            if (e1 % entry1SkipInterval == 0) {
                                var skipTo = Math.min(e1 + entry1SkipInterval, entry1.offsets.size() - 1);
                                var offset1Skip = entry1.offsets.get(skipTo);
                                if (offset1Skip < entry2.offsets.get(e2)) {
                                    e1 = skipTo;

                                    noSkips++;
                                    skipDistance += entry1SkipInterval;
                                }
                            }

                            // check if we can use some skips
                            if (e2 % entry2SkipInterval == 0) {
                                var skipTo = Math.min(e2 + entry2SkipInterval, entry2.offsets.size() - 1);
                                var offset2Skip = entry2.offsets.get(skipTo);
                                if (offset2Skip <= entry1.offsets.get(e1)) {
                                    e2 = skipTo;

                                    noSkips++;
                                    skipDistance += entry2SkipInterval;
                                }
                            }

                            var offset1 = entry1.offsets.get(e1);
                            var offset2 = entry2.offsets.get(e2);

                            if (offset2 == offset1 + 1) {
                                combined.offsets.add(offset2);
                            }
                        }
                    }

                    if (combined.offsets.size() != 0) {
                        answer.add(combined);
                    }

                    i++;
                    j++;
                } else if (entry1.docID < entry2.docID) {
                    // check if we can use some skips
                    if (i % totalAnswerSkipInterval == 0) {
                        var skipTo = Math.min(i + totalAnswerSkipInterval, totalAnswer.size() - 1);
                        var entry1Skip = totalAnswer.get(skipTo);
                        if (entry1Skip.docID <= entry2.docID) {
                            i = skipTo;
                        } else {
                            i++;
                        }
                    } else {
                        i++;
                    }

                } else {
                    // check if we can use some skips
                    if (j % queryListSkipInterval == 0) {
                        var skipTo = Math.min(j + totalAnswerSkipInterval, queryList.size() - 1);
                        var entry2Skip = queryList.get(skipTo);
                        if (entry2Skip.docID <= entry1.docID) {
                            j = skipTo;
                        } else {
                            j++;
                        }
                    } else {
                        j++;
                    }

                }
            }
            totalAnswer = answer;

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

    private PostingsList rankedQuery(Query query) {
        return null;
    }
}