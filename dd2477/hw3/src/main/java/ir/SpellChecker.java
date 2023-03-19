/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Dmytro Kalpakchi, 2018
 */

package ir;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;


public class SpellChecker {
    /**
     * The regular inverted index to be used by the spell checker
     */
    Index index;

    /**
     * K-gram index to be used by the spell checker
     */
    KGramIndex kgIndex;

    /**
     * The auxiliary class for containing the value of your ranking function for a token
     */
    class KGramStat implements Comparable {
        double score;
        String token;

        KGramStat(String token, double score) {
            this.token = token;
            this.score = score;
        }

        public String getToken() {
            return token;
        }

        public int compareTo(Object other) {
            if (this.score == ((KGramStat) other).score) return 0;
            return this.score < ((KGramStat) other).score ? -1 : 1;
        }

        public String toString() {
            return token + ";" + score;
        }
    }

    /**
     * The threshold for Jaccard coefficient; a candidate spelling
     * correction should pass the threshold in order to be accepted
     */
    private static final double JACCARD_THRESHOLD = 0.4;


    /**
     * The threshold for edit distance for a candidate spelling
     * correction to be accepted.
     */
    private static final int MAX_EDIT_DISTANCE = 2;


    public SpellChecker(Index index, KGramIndex kgIndex) {
        this.index = index;
        this.kgIndex = kgIndex;
    }

    /**
     * Computes the Jaccard coefficient for two sets A and B, where the size of set A is
     * <code>szA</code>, the size of set B is <code>szB</code> and the intersection
     * of the two sets contains <code>intersection</code> elements.
     */
    private double jaccard(int szA, int szB, int intersection) {
        var nonIntersection = szA + szB - intersection * 2;
        return (double) intersection / (double) nonIntersection;
    }

    /**
     * Computing Levenshtein edit distance using dynamic programming.
     * Allowed operations are:
     * => insert (cost 1)
     * => delete (cost 1)
     * => substitute (cost 2)
     */
    private int editDistance(String s1, String s2) {
        var m = s1.length();
        var n = s2.length();
        var tab = new int[m + 1][n + 1];

        // initialize the table (base cases)
        for (var i = 0; i <= m; i++) {
            tab[i][0] = i;
        }
        for (var i = 0; i <= n; i++) {
            tab[0][i] = i;
        }

        // dynamic programming
        for (var i = 1; i <= m; i++) {
            for (var j = 1; j <= n; j++) {
                var substitutionCost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 2;

                // minimum of three operations:
                //   - insert       (cost 1)
                //   - delete       (cost 1)
                //   - substitute   (cost 0 or 2)
                tab[i][j] = Math.min(tab[i - 1][j] + 1, Math.min(tab[i][j - 1] + 1, tab[i - 1][j - 1] + substitutionCost));
            }
        }

        // return the edit distance when the strings are at "full size"
        return tab[m][n];
    }

    /**
     * Checks spelling of all terms in <code>query</code> and returns up to
     * <code>limit</code> ranked suggestions for spelling correction.
     */
    public String[] check(Query query, int limit) {
        var queryTerm = query.queryterm.get(0);
        var kgrams = kgIndex.getKGrams(queryTerm.term);
        var maybeMatch = new ArrayList<KGramStat>();
        var cache = new HashSet<Integer>();

        for (var kgram : kgrams) {
            var kgramPostings = kgIndex.getPostings(kgram);
            for (var posting : kgramPostings) {
                var term = kgIndex.getTermByID(posting.tokenID);
                if (cache.contains(posting.tokenID)) continue;
                cache.add(posting.tokenID);

                var termKgrams = kgIndex.getKGrams(term);
                var intersection = new ArrayList<>(kgrams);
                intersection.retainAll(termKgrams);

                var score = jaccard(kgrams.size(), termKgrams.size(), intersection.size());
                if (score > JACCARD_THRESHOLD) {
                    maybeMatch.add(new KGramStat(term, score));
                }
            }
        }

        var finalMatch = new ArrayList<KGramStat>();
        for (var match : maybeMatch) {
            var editDistance = editDistance(queryTerm.term, match.token);
            if (editDistance <= MAX_EDIT_DISTANCE) {
                finalMatch.add(new KGramStat(match.token, match.score));
            }
        }

        finalMatch.sort((o1, o2) -> Double.compare(o2.score, o1.score));

        return finalMatch.subList(0, Math.min(limit, finalMatch.size())).stream().map(KGramStat::getToken).toArray(String[]::new);
    }

    /**
     * Merging ranked candidate spelling corrections for all query terms available in
     * <code>qCorrections</code> into one final merging of query phrases. Returns up
     * to <code>limit</code> corrected phrases.
     */
    private List<KGramStat> mergeCorrections(List<List<KGramStat>> qCorrections, int limit) {


        return null;
    }
}
