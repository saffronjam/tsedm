/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, 2017
 */

package ir;

import java.util.HashMap;
import java.util.Iterator;

/**
 * Implements an inverted index as a Hashtable from words to PostingsLists.
 */
public class HashedIndex implements Index {

    /** The index as a hashtable. */
    private HashMap<String, PostingsList> index = new HashMap<String, PostingsList>();

    /**
     * Inserts this token in the hashtable.
     */
    public void insert(String token, int docID, int offset) {
        var list = index.get(token);
        if (list == null) {
            list = new PostingsList();
        }

        list.add(docID, 0, offset);

        index.put(token, list);
    }

    /**
     * Returns the postings for a specific term, or null
     * if the term is not in the index.
     */
    public PostingsList getPostings(String token) {
        if (index.containsKey(token)) {
            return index.get(token);
        }
        return new PostingsList();
    }

    /**
     * No need for cleanup in a HashedIndex.
     */
    public void cleanup() {
    }
}
