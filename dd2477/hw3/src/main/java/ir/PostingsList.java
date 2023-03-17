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

    public static PostingsList merge(PostingsList postingsList1, PostingsList postingsList2) {
        var mergedList = new PostingsList(postingsList1.getToken());
        var i = 0;
        var j = 0;

        while (i < postingsList1.size() && j < postingsList2.size()) {
            var entry1 = postingsList1.get(i);
            var entry2 = postingsList2.get(j);

            if (entry1.docID == entry2.docID) {
                // if docIds are equal, merge the entries
                var mergedEntry = new PostingsEntry(entry1.docID, entry1.score + entry2.score);
                mergedEntry.offsets.addAll(entry1.offsets);
                mergedEntry.offsets.addAll(entry2.offsets);
                mergedList.add(mergedEntry);
                i++;
                j++;
            } else if (entry1.docID < entry2.docID) {
                // if docId of entry1 is smaller, add it to the merged list
                mergedList.add(entry1);
                i++;
            } else {
                // if docId of entry2 is smaller, add it to the merged list
                mergedList.add(entry2);
                j++;
            }
        }

        // add the remaining entries
        while (i < postingsList1.size()) {
            mergedList.add(postingsList1.get(i));
            i++;
        }

        while (j < postingsList2.size()) {
            mergedList.add(postingsList2.get(j));
            j++;
        }

        return mergedList;
    }
}
