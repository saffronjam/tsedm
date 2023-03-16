/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Johan Boye, 2017
 */

package ir;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

/**
 * A class for representing a query as a list of words, each of which has
 * an associated weight.
 */
public class Query {

    /**
     * Help class to represent one query term, with its associated weight.
     */
    class QueryTerm {
        String term;
        double weight;

        QueryTerm(String t, double w) {
            term = t;
            weight = w;
        }

        @Override
        public String toString() {
            return "[" + term + " " + weight + "]";
        }
    }

    /**
     * Representation of the query as a list of terms with associated weights.
     * In assignments 1 and 2, the weight of each term will always be 1.
     */
    public ArrayList<QueryTerm> queryterm = new ArrayList<QueryTerm>();

    /**
     * Relevance feedback constant alpha (= weight of original query terms).
     * Should be between 0 and 1.
     * (only used in assignment 3).
     */
    double alpha = 0.2;

    /**
     * Relevance feedback constant beta (= weight of query terms obtained by
     * feedback from the user).
     * (only used in assignment 3).
     */
    double beta = 1 - alpha;

    /**
     * Creates a new empty Query
     */
    public Query() {
    }

    /**
     * Creates a new Query from a string of words
     */
    public Query(String queryString) {
        StringTokenizer tok = new StringTokenizer(queryString);
        while (tok.hasMoreTokens()) {
            queryterm.add(new QueryTerm(tok.nextToken(), 1.0));
        }
    }

    /**
     * Returns the number of terms
     */
    public int size() {
        return queryterm.size();
    }

    /**
     * Returns the Manhattan query length
     */
    public double length() {
        double len = 0;
        for (QueryTerm t : queryterm) {
            len += t.weight;
        }
        return len;
    }

    /**
     * Returns a copy of the Query
     */
    public Query copy() {
        Query queryCopy = new Query();
        for (QueryTerm t : queryterm) {
            queryCopy.queryterm.add(new QueryTerm(t.term, t.weight));
        }
        return queryCopy;
    }

    /**
     * Expands the Query using Relevance Feedback
     *
     * @param results       The results of the previous query.
     * @param docIsRelevant A boolean array representing which query results the
     *                      user deemed relevant.
     * @param engine        The search engine object
     */
    public void relevanceFeedback(PostingsList results, boolean[] docIsRelevant, Engine engine) {
        if (engine.gui.queryType == QueryType.RANKED_QUERY) {
            var rocchioTerms = new HashMap<String, Double>();

            for (var term : queryterm) {
                rocchioTerms.put(term.term, rocchioTerms.getOrDefault(term.term, 0.0) + term.weight * alpha);
            }

            for (int i = 0; i < docIsRelevant.length; i++) {
                var docId = results.get(i).docID;

                if (docIsRelevant[i]) {
                    // put doc terms into beta terms
                    var docTerms = getDocTerms(docId);

                    var tfs = new HashMap<String, Integer>();
                    for (var term : docTerms) {
                        tfs.put(term, tfs.getOrDefault(term, 0) + 1);
                    }


                    for (var tf : tfs.entrySet()) {
                        rocchioTerms.put(tf.getKey(), rocchioTerms.getOrDefault(tf.getKey(), 0.0) + tf.getValue() * beta);
                    }
                }
            }

            queryterm.clear();

            // add beta terms to query
            for (var entry : rocchioTerms.entrySet()) {
                queryterm.add(new QueryTerm(entry.getKey(), entry.getValue()));
            }
        }
    }

    private ArrayList<String> getDocTerms(int docId) {
        var result = new ArrayList<String>();

        var docPath = Index.docNames.get(docId);
        File f = new File(docPath);

        try (Reader reader = new InputStreamReader(new FileInputStream(f), StandardCharsets.UTF_8)) {
            Tokenizer tok = new Tokenizer(reader, true, false, true, "patterns.txt");
            while (tok.hasMoreTokens()) {
                String token = tok.nextToken();
                result.add(token);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return result;
    }
}
