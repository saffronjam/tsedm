/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, 2017
 */

package ir;

import java.util.ArrayList;
import java.util.Arrays;
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
                return totalAnswer;
            }
        }

        return totalAnswer;
    }

    private PostingsList phraseQuery(Query query) {
        var queryPostingsList = new ArrayList<PostingsList>();

        // step 1: collect all posting list for individual terms
        for (var queryTerm : query.queryterm) {
            queryPostingsList.add(index.getPostings(queryTerm.term));
        }

        // step 2: modified intersection algorithm with the rest
        var totalAnswer = new PostingsList(queryPostingsList.get(0));

        for (int queryListIndex = 1; queryListIndex < queryPostingsList.size(); queryListIndex++) {
            var queryList = queryPostingsList.get(queryListIndex);

            int i = 0, j = 0;

            var answer = new PostingsList(queryList.getToken());
            while (i < totalAnswer.size() && j < queryList.size()) {
                var entry1 = totalAnswer.get(i);
                var entry2 = queryList.get(j);
                var combined = new PostingsEntry(entry1.docID, 0);

                if (entry1.docID == entry2.docID) {
                    for (var offset1 : entry1.offsets) {
                        combined.offsets.addAll((ArrayList<Integer>) entry2.offsets
                                .stream()
                                .filter(offset2 -> offset1 == offset2 - 1)
                                .collect(Collectors.toList()));
                    }

                    if (combined.offsets.size() != 0) {
                        answer.add(combined);
                    }

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
                return totalAnswer;
            }
        }

        return totalAnswer;
    }

    private PostingsList rankedQuery(Query query) {
        return null;
    }
}