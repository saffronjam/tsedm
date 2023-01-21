/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, KTH, 2018
 */

package ir;

import java.io.*;
import java.util.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.nio.charset.*;
import java.nio.file.Path;

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
public class PersistentHashedIndex implements Index {

    /** The directory where the persistent index files are stored. */
    public static final String INDEXDIR = "index/";

    /** The dictionary file name */
    public static final String DICTIONARY_FNAME = "dictionary";

    /** The data file name */
    public static final String DATA_FNAME = "data";

    /** The terms file name */
    public static final String TERMS_FNAME = "terms";

    /** The doc info file name */
    public static final String DOCINFO_FNAME = "docInfo";

    /** The dictionary hash table on disk can fit this many entries. */
    public static final long TABLESIZE = 611953L;

    /** The dictionary hash table is stored in this file. */
    RandomAccessFile dictionaryFile;

    /** The data (the PostingsLists) are stored in this file. */
    RandomAccessFile dataFile;

    /** Pointer to the first free memory cell in the data file. */
    long free = 0L;

    /** The cache as a main-memory hash map. */
    HashMap<String, PostingsList> index = new HashMap<String, PostingsList>();

    // ===================================================================

    public static final String BASE_DIR = "grade-b/";

    public static final boolean DELETE_ON_START = false;

    /**
     * A helper class representing one entry in the dictionary hashtable.
     */
    public class Entry {
        public static final int BYTE_SIZE = 20;

        public long verify;
        public long ptr;
        public int size;
    }

    private static void createFile(String filename) throws IOException {
        var file = new File(filename);
        file.getParentFile().mkdirs();
        file.createNewFile();
    }

    // ==================================================================

    /**
     * Constructor. Opens the dictionary file and the data file.
     * If these files don't exist, they will be created.
     */
    public PersistentHashedIndex() {
        try {
            var dictionaryFilename = BASE_DIR + INDEXDIR + DICTIONARY_FNAME;
            var dataFilename = BASE_DIR + INDEXDIR + DATA_FNAME;
            var docInfoFilename = BASE_DIR + INDEXDIR + DOCINFO_FNAME;

            if (DELETE_ON_START) {
                new File(dictionaryFilename).delete();
                new File(dataFilename).delete();
                new File(docInfoFilename).delete();
            }

            createFile(dictionaryFilename);
            createFile(dataFilename);
            createFile(docInfoFilename);

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

    /**
     * Writes data to the data file at a specified place.
     *
     * @return The number of bytes written.
     */
    int writeData(String dataString, long ptr) {
        try {
            dataFile.seek(ptr);
            byte[] data = dataString.getBytes();
            dataFile.write(data);
            return data.length;
        } catch (IOException e) {
            e.printStackTrace();
            return -1;
        }
    }

    /**
     * Reads data from the data file
     */
    String readData(long ptr, int size) {
        try {
            dataFile.seek(ptr);
            byte[] data = new byte[size];
            dataFile.readFully(data);
            return new String(data);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    // ==================================================================
    //
    // Reading and writing to the dictionary file.

    /*
     * Writes an entry to the dictionary hash table file.
     * 
     * @param entry The key of this entry is assumed to have a fixed length
     * 
     * @param ptr The place in the dictionary file to store the entry
     */
    void writeEntry(Entry entry, long ptr) {
        try {
            dictionaryFile.seek(ptr);
            dictionaryFile.writeLong(entry.verify);

            dictionaryFile.seek(ptr + 8);
            dictionaryFile.writeLong(entry.ptr);

            dictionaryFile.seek(ptr + 16);
            dictionaryFile.writeInt(entry.size);

        } catch (IOException ioException) {
            System.err.println("failed to write entry: " + ioException.getMessage());
        }
    }

    /**
     * Reads an entry from the dictionary file.
     *
     * @param ptr The place in the dictionary file where to start reading.
     */
    Entry readEntry(long ptr) {
        try {
            var entry = new Entry();

            dictionaryFile.seek(ptr);
            entry.verify = dictionaryFile.readLong();

            dictionaryFile.seek(ptr + 8);
            entry.ptr = dictionaryFile.readLong();

            dictionaryFile.seek(ptr + 16);
            entry.size = dictionaryFile.readInt();

            return entry;
        } catch (IOException ioException) {
            System.err.println("failed to read entry: " + ioException.getMessage());
        }
        return null;
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

    /**
     * Reads the document names and document lengths from file, and
     * put them in the appropriate data structures.
     *
     * @throws IOException { exception_description }
     */
    private void readDocInfo() throws IOException {
        File file = new File(BASE_DIR + INDEXDIR + DOCINFO_FNAME);
        FileReader freader = new FileReader(file);
        try (BufferedReader br = new BufferedReader(freader)) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] data = line.split(";");
                docNames.put(new Integer(data[0]), data[1]);
                docLengths.put(new Integer(data[0]), new Integer(data[2]));
            }
        }
        freader.close();
    }

    // ==================================================================

    /**
     * Write the index to files.
     */
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
                int sq = 1;
                while (isColliding(hash * Entry.BYTE_SIZE, 0)) {
                    hash = (hash += sq) % TABLESIZE;
                    collisions++;
                    sq = sq * 2;
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

    /*
     * Return an ptr which is essentially an index to the dictionary created by
     * hashing the token
     */
    private long getHashLocation(String token) {
        long hashResult = 0;
        for (int i = 0; i < token.length(); i++) {
            // mod TABLESIZE to wrap our limit space (no index out of bounds)
            hashResult = (31 * hashResult + token.charAt(i)) % TABLESIZE;
        }
        return hashResult;
    }

    private long getHashVerify(String token) {
        long hashResult = 0;
        for (int i = 0; i < token.length(); i++) {
            // mod TABLESIZE to wrap our limit space (no index out of bounds)
            hashResult = (37 * hashResult + token.charAt(i)) % TABLESIZE;
        }
        return hashResult;
    }

    private boolean isColliding(long ptr, long verify) {
        var entry = readEntry(ptr);
        var colliding = verify != entry.verify;
        return colliding;
    }

    /**
     * Returns the postings for a specific term, or null
     * if the term is not in the index.
     */
    public PostingsList getPostings(String token) {
        // step 1: look up in dictionary to get ptr
        long hash = getHashLocation(token);
        long verify = getHashVerify(token);
        while (isColliding(hash * Entry.BYTE_SIZE, verify)) {
            hash = (hash + 1) % TABLESIZE;
        }
        var entry = readEntry(hash * Entry.BYTE_SIZE);
        var dataPtr = entry.ptr;

        // step 2: load data at ptr
        var rawData = readData(dataPtr, entry.size);

        // step 3: parse data into PostingsList
        var postingsList = new PostingsList();

        var postingsListParts = rawData.split("\\|");

        for (var postingsListPart : postingsListParts) {
            var postingsEntryParts = postingsListPart.split(";");

            // docID, score, offset
            var docID = Integer.valueOf(postingsEntryParts[0]);
            var score = Double.valueOf(postingsEntryParts[1]);
            var offsets = (ArrayList<Integer>) Arrays
                    .asList(postingsEntryParts[2].replaceAll("\\[|\\]", "").split(", "))
                    .stream()
                    .map(offset -> Integer.valueOf(offset))
                    .collect(Collectors.toList());

            var postingsEntry = new PostingsEntry(docID, score);
            postingsEntry.offsets.addAll(offsets);

            postingsList.add(postingsEntry);
        }

        return postingsList;
    }

    /**
     * Inserts this token in the main-memory hashtable.
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
     * Write index to file after indexing is done.
     */
    public void cleanup() {
        System.err.println(index.keySet().size() + " unique words");
        System.err.print("Writing index to disk...");
        writeIndex();
        System.err.println("done!");
    }
}
