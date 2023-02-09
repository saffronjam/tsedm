/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Johan Boye, 2017
 */

package ir;

import java.util.HashMap;
import java.util.List;

/**
 * Searches an index for results of a query.
 */
public class Searcher {

    /**
     * The index to be searched by this Searcher.
     */
    Index index;

    /**
     * The k-gram index to be searched by this Searcher
     */
    KGramIndex kgIndex;

    /**
     * Constructor
     */
    public Searcher(Index index, KGramIndex kgIndex) {
        this.index = index;
        this.kgIndex = kgIndex;
    }

    int skips = 0;
    int skipDistance = 0;

    int docIdSkips = 0;
    int docIdskipDistance = 0;

    /**
     * Searches the index for postings matching the query.
     *
     * @return A postings list representing the result of the query.
     */
    public PostingsList search(Query query, QueryType queryType, RankingType rankingType, NormalizationType normType) {

        skips = 0;
        skipDistance = 0;
        docIdSkips = 0;
        docIdskipDistance = 0;

        var list = switch (queryType) {
            case INTERSECTION_QUERY -> intersectionQuery(query);
            case PHRASE_QUERY -> phraseQuery(query);
            case RANKED_QUERY -> rankedQuery(query);
        };

        System.out.println("docID skips: " + docIdSkips);
        System.out.println("docID skip distance: " + docIdskipDistance);
        System.out.println("skips: " + skips);
        System.out.println("skip distance: " + skipDistance);

        return list;
    }

    private PostingsList intersectionQuery(Query query) {
        var cache = new HashMap<String, PostingsList>();

        if (query.size() == 0) {
            return null;
        }

        var start = System.currentTimeMillis();
        var parsing = 0L;

        var initialTerm = query.queryterm.get(0).term;

        var parsingStart = System.currentTimeMillis();
        var totalAnswer = index.getPostings(initialTerm);
        var parsingEnd = System.currentTimeMillis();
        parsing += (parsingEnd - parsingStart);

        cache.put(initialTerm, totalAnswer);

        for (int i = 1; i < query.size(); i++) {
            var token = query.queryterm.get(i).term;

            var cachedList = cache.get(token);
            if (cachedList == null) {
                parsingStart = System.currentTimeMillis();
                cachedList = index.getPostings(token);
                parsingEnd = System.currentTimeMillis();
                parsing += (parsingEnd - parsingStart);

                cache.put(token, cachedList);
            } else {
                continue;
            }

            totalAnswer = intersectComparison(totalAnswer, cachedList);

            if (totalAnswer.size() == 0) {
                break;
            }
        }

        var end = System.currentTimeMillis();

        System.out.println(
                "intersection query took: " + (end - start) + " ms (" + ((end - start) - parsing)
                        + " ms without parsing)");

        return totalAnswer;
    }

    private PostingsList intersectComparison(PostingsList p1, PostingsList p2) {
        var answer = makeIntersectionQuery(p1, p2, (entry1, entry2) -> entry1);

        return answer;
    }

    private PostingsList phraseQuery(Query query) {
        var cache = new HashMap<String, PostingsList>();

        if (query.size() == 0) {
            return null;
        }

        var start = System.currentTimeMillis();
        var parsing = 0L;

        var intialTerm = query.queryterm.get(0).term;

        var parsingStart = System.currentTimeMillis();
        var totalAnswer = index.getPostings(intialTerm);
        var parsingEnd = System.currentTimeMillis();
        parsing += (parsingEnd - parsingStart);

        cache.put(intialTerm, totalAnswer);

        for (int i = 1; i < query.size(); i++) {
            var token = query.queryterm.get(i).term;

            var cachedList = cache.get(token);
            if (cachedList == null) {
                parsingStart = System.currentTimeMillis();
                cachedList = index.getPostings(token);
                parsingEnd = System.currentTimeMillis();
                parsing += (parsingEnd - parsingStart);

                cache.put(token, cachedList);
            }

            totalAnswer = phraseComparison(totalAnswer, cachedList);

            if (totalAnswer.size() == 0) {
                break;
            }
        }

        var end = System.currentTimeMillis();

        System.out.println(
                "phrase query took: " + (end - start) + " ms (" + ((end - start) - parsing) + " ms without parsing)");

        return totalAnswer;
    }

    private PostingsList phraseComparison(PostingsList p1, PostingsList p2) {
        var answer = makeIntersectionQuery(p1, p2, (entry1, entry2) -> {
            var combined = new PostingsEntry(entry1.docID, 0);

            var skipInterval1 = (int) Math.sqrt(entry1.offsets.size());
            var skipInterval2 = (int) Math.sqrt(entry2.offsets.size());

            int i = 0, j = 0;
            while (i < entry1.offsets.size() && j < entry2.offsets.size()) {
                while (canSkipOffsets(i, skipInterval1, entry2.offsets.get(j), entry1.offsets)) {
                    i += skipInterval1;

                    skips++;
                    skipDistance += skipInterval1;
                }

                while (canSkipOffsets(j, skipInterval2, entry1.offsets.get(i), entry2.offsets)) {
                    j += skipInterval2;

                    skips++;
                    skipDistance += skipInterval2;
                }

                var offset1 = entry1.offsets.get(i);
                var offset2 = entry2.offsets.get(j);

                if (offset2 == offset1 + 1) {
                    combined.offsets.add(offset2);

                    i++;
                    j++;
                } else if (offset1 < offset2) {
                    i++;
                } else {
                    j++;
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

            while (canSkipDocuments(i, p1SkipInterval, entry2.docID, p1)) {
                i += p1SkipInterval;

                docIdSkips++;
                docIdskipDistance += p1SkipInterval;
            }

            while (canSkipDocuments(j, p2SkipInterval, entry1.docID, p2)) {
                j += p2SkipInterval;

                docIdSkips++;
                docIdskipDistance += p1SkipInterval;
            }

            if (entry1.docID == entry2.docID) {

                var result = handler.onMatch(entry1, entry2);
                if (result != null) {
                    answer.add(result);
                }

                i++;
                j++;
            } else if (entry1.docID < entry2.docID) {
                i++;
            } else {
                j++;
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
        return offset1Skip < otherOffset - 1;
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
        return entry2Skip.docID <= otherDocId;
    }

    private PostingsList rankedQuery(Query query) {
        var cache = new HashMap<String, PostingsList>();

        if (query.size() == 0) {
            return null;
        }

        var start = System.currentTimeMillis();
        var parsing = 0L;

        var answer = new PostingsList("");

        for (int i = 0; i < query.size(); i++) {
            var token = query.queryterm.get(i).term;

            var cachedList = cache.get(token);
            if (cachedList == null) {
                var parsingStart = System.currentTimeMillis();
                cachedList = index.getPostings(token);
                var parsingEnd = System.currentTimeMillis();
                parsing += (parsingEnd - parsingStart);

                cache.put(token, cachedList);
            } else {
                continue;
            }

            for (int j = 0; j < cachedList.size(); j++) {
                var entry = cachedList.get(j);

                var tf = cachedList.get(j).offsets.size();

                // idf = log(N/df)
                var idf = Math.log((double) index.docNames.size() / cachedList.size());

                var score = tf * idf / index.docLengths.get(entry.docID);

                entry.score = score;

                answer.add(entry.docID, entry.score, entry.offsets);
            }
        }

        answer.sortEntriesByScore();

        var end = System.currentTimeMillis();

        System.out.println(
                "ranked query took: " + (end - start) + " ms (" + ((end - start) - parsing)
                        + " ms without parsing)");

        return answer;
    }
}