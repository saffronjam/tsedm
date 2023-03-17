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




    public static PostingsEntry merge(PostingsEntry postingsEntry1, PostingsEntry postingsEntry2) {
        var merged = new PostingsEntry(postingsEntry1.docID, postingsEntry1.score + postingsEntry2.score);

        if (postingsEntry1.offsets.size() == 0) {
            return postingsEntry2;
        }
        if (postingsEntry2.offsets.size() == 0) {
            return postingsEntry1;
        }
        int i = 0;
        int j = 0;
        while (i < postingsEntry1.offsets.size() && j < postingsEntry2.offsets.size()) {
            if (postingsEntry1.offsets.get(i) < postingsEntry2.offsets.get(j)) {
                merged.offsets.add(postingsEntry1.offsets.get(i));
                i++;
            } else {
                merged.offsets.add(postingsEntry2.offsets.get(j));
                j++;
            }
        }
        while (i < postingsEntry1.offsets.size()) {
            merged.offsets.add(postingsEntry1.offsets.get(i));
            i++;
        }
        while (j < postingsEntry2.offsets.size()) {
            merged.offsets.add(postingsEntry2.offsets.get(j));
            j++;
        }
        return merged;
    }

}
