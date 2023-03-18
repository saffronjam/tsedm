/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Johan Boye, 2017
 */

package ir;

import java.io.Serializable;
import java.util.ArrayList;

public class PostingsEntry implements Comparable<PostingsEntry>, Serializable {

    public int docID;
    public double score = 0;

    public PostingsEntry(int docID, double score) {
        this.docID = docID;
        this.score = score;
    }

    /**
     * PostingsEntries are compared by their score (only relevant
     * in ranked retrieval).
     * <p>
     * The comparison is defined so that entries will be put in
     * descending order.
     */
    public int compareTo(PostingsEntry other) {
        return Double.compare(other.score, score);
    }


    public ArrayList<Integer> offsets = new ArrayList<>();




    public static PostingsEntry merge(PostingsEntry p1, PostingsEntry p2) {
        var merged = new PostingsEntry(p1.docID, p1.score + p2.score);

        if (p1.offsets.size() == 0) {
            return p2;
        }
        if (p2.offsets.size() == 0) {
            return p1;
        }
        int i = 0;
        int j = 0;
        while (i < p1.offsets.size() && j < p2.offsets.size()) {
            if (p1.offsets.get(i) < p2.offsets.get(j)) {
                merged.offsets.add(p1.offsets.get(i));
                i++;
            } else {
                merged.offsets.add(p2.offsets.get(j));
                j++;
            }
        }
        while (i < p1.offsets.size()) {
            merged.offsets.add(p1.offsets.get(i));
            i++;
        }
        while (j < p2.offsets.size()) {
            merged.offsets.add(p2.offsets.get(j));
            j++;
        }
        return merged;
    }

}
