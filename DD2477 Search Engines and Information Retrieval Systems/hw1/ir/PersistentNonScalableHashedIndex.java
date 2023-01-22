/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, KTH, 2018
 */

package ir;

import java.io.*;
import java.util.*;

/*
 *   Implements an inverted index as a hashtable on disk.
 *   
 *   Both the words (the dictionary) and the data (the postings list) are
 *   stored in RandomAccessFiles that permit fast (almost constant-time)
 *   disk seeks. 
 *
 *   When words are read and indexed, they are first put in an ordinary,
 *   main-memory HashMap. When all words are read, the index is committed
 *   to disk.
 */
public class PersistentNonScalableHashedIndex extends PersistentHashedIndex {

    public static final String BASE_DIR = "grade-b/";

    public PersistentNonScalableHashedIndex() {
        super(BASE_DIR);
    }

    // ==================================================================

    /**
     * Writes the document names and document lengths to file.
     *
     * @throws IOException { exception_description }
     */
    private void writeDocInfo() throws IOException {
        FileOutputStream fout = new FileOutputStream(BASE_DIR + INDEXDIR + DOCINFO_FNAME);
        for (Map.Entry<Integer, String> entry : docNames.entrySet()) {
            Integer key = entry.getKey();
            String docInfoEntry = key + ";" + entry.getValue() + ";" + docLengths.get(key) + "\n";
            fout.write(docInfoEntry.getBytes());
        }
        fout.close();
    }

    // ==================================================================

    @Override
    public void writeIndex() {
        int collisions = 0;
        try {
            // Write the 'docNames' and 'docLengths' hash maps to a file
            writeDocInfo();

            // Write the dictionary and the postings list

            long dataPtr = 0;
            for (var indexEntry : index.entrySet()) {

                var token = indexEntry.getKey();
                var postingsList = indexEntry.getValue();

                // step 1: convert PostingsList to string
                var stringBuilder = new StringBuilder();
                for (int i = 0; i < postingsList.size(); i++) {
                    stringBuilder
                            .append(postingsList.get(i).docID)
                            .append(';')
                            .append(postingsList.get(i).score)
                            .append(';')
                            .append(Arrays.toString(postingsList.get(i).offsets.toArray()))
                            .append("|");
                }

                var stringData = stringBuilder.toString();

                // step 2: write to data to know ptr in dictionary
                var bytesWritten = writeData(stringData, dataPtr);

                // step 3: hash the token and check if we need to change place in dictionary
                long hash = getHashLocation(token);
                long verify = getHashVerify(token);
                // verify is 0 since we have not written anything yet
                int diff = 1;
                while (isColliding(hash * Entry.BYTE_SIZE, 0)) {
                    hash = (hash += diff) % TABLESIZE;
                    diff = diff * 2;
                    collisions++;
                }

                // step 4: write to dictionary with ptr from step 2
                var entry = new Entry();
                entry.ptr = dataPtr;
                entry.verify = verify;
                entry.size = bytesWritten;

                writeEntry(entry, hash * Entry.BYTE_SIZE);

                dataPtr += bytesWritten;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.err.println(collisions + " collisions.");
    }

    @Override
    public void insert(String token, int docID, int offset) {
        var list = index.get(token);
        if (list == null) {
            list = new PostingsList();
        }

        list.add(docID, 0, offset);

        index.put(token, list);
    }

    @Override
    public void cleanup() {
        System.err.println(index.keySet().size() + " unique words");
        System.err.print("Writing index to disk...");
        writeIndex();
        System.err.println("done!");
    }
}
