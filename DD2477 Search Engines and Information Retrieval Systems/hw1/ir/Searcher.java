/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, 2017
 */

package ir;

import java.util.ArrayList;
import java.util.Arrays;

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
        for (var queryTerm : query.queryterm) {
            queryPostingsList.add(index.getPostings(queryTerm.term));
        }

        if (queryPostingsList.size() == 0) {
            return null;
        }

        // step 2: intersection algorithm with rest

        var totalAnswer = new PostingsList();

        for (int i = 0; i < queryPostingsList.get(0).size(); i++) {
            totalAnswer.add(queryPostingsList.get(0).get(i));
        }

        for (int queryListIndex = 1; queryListIndex < queryPostingsList.size(); queryListIndex++) {
            var queryList = queryPostingsList.get(queryListIndex);

            int i = 0, j = 0;

            var answer = new PostingsList();
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
        }

        return totalAnswer;
    }

    private PostingsList phraseQuery(Query query) {
        return null;
    }

    private PostingsList rankedQuery(Query query) {
        return null;
    }
}