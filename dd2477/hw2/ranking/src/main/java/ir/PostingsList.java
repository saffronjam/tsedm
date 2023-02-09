/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Johan Boye, 2017
 */

package ir;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

public class PostingsList {

    /**
     * The postings list
     */
    private final ArrayList<PostingsEntry> list = new ArrayList<PostingsEntry>();
    private final HashMap<Integer, PostingsEntry> map = new HashMap<Integer, PostingsEntry>();
    private String token;

    public PostingsList(String token) {
        this.token = token;
    }

    /**
     * Number of postings in this list.
     */
    public int size() {
        return list.size();
    }

    /**
     * Returns the ith posting.
     */
    public PostingsEntry get(int i) {
        return list.get(i);
    }

    public String getToken() {
        return token;
    }

    public void setToken(String token) {
        this.token = token;
    }

    public HashMap<Integer, PostingsEntry> getMap() {
        return map;
    }

    /**
     * Returns posting by docId.
     */
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

    public void add(int docID, double score, List<Integer> offsets) {
        var entry = map.get(docID);

        if (entry == null) {
            // if entry did not exist, create a new one
            var newEntry = new PostingsEntry(docID, score);
            newEntry.offsets.addAll(offsets);
            list.add(newEntry);
            map.put(docID, newEntry);
        } else {
            // if entry did exist, just add the offsets
            entry.offsets.addAll(offsets);
            entry.score += score;
        }
    }

    public void add(PostingsEntry entry) {
        list.add(entry);
        map.put(entry.docID, entry);
    }

    public void sortEntriesByDocId() {
        list.sort(Comparator.comparingInt(o -> o.docID));
    }

    public void sortEntriesByScore() {
        list.sort(PostingsEntry::compareTo);
    }
}
