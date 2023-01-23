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

        try {
            var dictionaryFilename = BASE_DIR + INDEXDIR + DICTIONARY_FNAME;
            var dataFilename = BASE_DIR + INDEXDIR + DATA_FNAME;
            var docInfoFilename = BASE_DIR + INDEXDIR + DOCINFO_FNAME;

            if (DELETE_ON_START) {
                new File(dictionaryFilename).delete();
                new File(dataFilename).delete();
                new File(docInfoFilename).delete();
            }

            createDirsAndFile(dictionaryFilename);
            createDirsAndFile(dataFilename);
            createDirsAndFile(docInfoFilename);

            dictionaryFile = new RandomAccessFile(dictionaryFilename, "rw");
            dataFile = new RandomAccessFile(dataFilename, "rw");

            // write in the end to allocate the space that will be used
            var end = Entry.BYTE_SIZE * TABLESIZE;
            dictionaryFile.seek(end);
            dictionaryFile.writeByte(0);

        } catch (IOException e) {
            e.printStackTrace();
        }

        try {
            readDocInfo();
        } catch (FileNotFoundException e) {
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // ==================================================================

    @Override
    protected void writeDocInfo() throws IOException {
        FileOutputStream fout = new FileOutputStream(BASE_DIR + INDEXDIR + DOCINFO_FNAME);
        for (Map.Entry<Integer, String> entry : docNames.entrySet()) {
            Integer key = entry.getKey();
            String docInfoEntry = key + ";" + entry.getValue() + ";" + docLengths.get(key) + "\n";
            fout.write(docInfoEntry.getBytes());
        }
        fout.close();
    }

    @Override
    protected void readDocInfo() throws IOException {
        File file = new File(BASE_DIR + INDEXDIR + DOCINFO_FNAME);
        FileReader freader = new FileReader(file);
        try (BufferedReader br = new BufferedReader(freader)) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] data = line.split(";");
                docNames.put(Integer.valueOf(data[0]), data[1]);
                docLengths.put(Integer.valueOf(data[0]), Integer.valueOf(data[2]));
            }
        }
        freader.close();
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
                var stringData = marshallPostingsList(postingsList);

                // step 2: write to data to know ptr in dictionary
                var bytesWritten = writeData(stringData, dataPtr);

                // step 3: hash the token and check if we need to change place in dictionary
                var hash = getHashLocation(token);
                var findResult = getFirstFreeDictSpace(hash * Entry.BYTE_SIZE);
                collisions += findResult.collisions;

                // step 4: write to dictionary with ptr from step 2
                var entry = new Entry();
                entry.ptr = dataPtr;
                entry.size = bytesWritten;

                writeEntry(entry, findResult.ptr);

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
            list = new PostingsList(token);
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
