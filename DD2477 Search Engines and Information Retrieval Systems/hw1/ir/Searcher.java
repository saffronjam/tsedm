/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, 2017
 */

package ir;

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
                break;
            case PHRASE_QUERY:
                if (query.queryterm.size() > 0) {
                    return index.getPostings(query.queryterm.get(0).term);
                }
                break;
            case RANKED_QUERY:
                break;
            default:
                break;
        }

        return null;
    }
}