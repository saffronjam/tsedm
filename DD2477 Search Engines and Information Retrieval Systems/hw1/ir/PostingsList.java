/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, 2017
 */

package ir;

import java.util.ArrayList;
import java.util.HashMap;

public class PostingsList {

    /** The postings list */
    private ArrayList<PostingsEntry> list = new ArrayList<PostingsEntry>();
    private HashMap<Integer, PostingsEntry> map = new HashMap<Integer, PostingsEntry>();

    public PostingsList() {

    }

    public PostingsList(PostingsList copy) {
        list.addAll(copy.list);
    }

    /** Number of postings in this list. */
    public int size() {
        return list.size();
    }

    /** Returns the ith posting. */
    public PostingsEntry get(int i) {
        return list.get(i);
    }

    /** Returns posting by docId. */
    public PostingsEntry getByDocId(int docId) {
        return map.get(docId);
    }

    public void add(int docID, double score, int offset) {
        var entry = map.get(docID);

        if (entry == null) {
            // if entry did not exist, create a new one
            var newEntry = new PostingsEntry(docID, score);
            newEntry.offsets.add(offset);
            list.add(newEntry);
            map.put(docID, newEntry);
        } else {
            // if entry did exist, just add the offset
            entry.offsets.add(offset);
        }
    }

    public void add(PostingsEntry entry) {
        list.add(entry);
    }
}
