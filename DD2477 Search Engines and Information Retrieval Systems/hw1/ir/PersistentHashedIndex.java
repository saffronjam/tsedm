/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, KTH, 2018
 */

package ir;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

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
public abstract class PersistentHashedIndex implements Index {

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

    public static final boolean DELETE_ON_START = false;

    public String BASE_DIR;

    /**
     * A helper class representing one entry in the dictionary hashtable.
     */
    public class Entry {
        public static final int BYTE_SIZE = 20;

        public long verify;
        public long ptr;
        public int size;

        public boolean valid() {
            return !(verify == 0 && ptr == 0 && size == 0);
        }
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
    public PersistentHashedIndex(String baseDir) {
        this.BASE_DIR = baseDir;

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
                docNames.put(Integer.valueOf(data[0]), data[1]);
                docLengths.put(Integer.valueOf(data[0]), Integer.valueOf(data[2]));
            }
        }
        freader.close();
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

    /*
     * Return an ptr which is essentially an index to the dictionary created by
     * hashing the token
     */
    protected long getHashLocation(String token) {
        long hashResult = 0;
        for (int i = 0; i < token.length(); i++) {
            // mod TABLESIZE to wrap our limit space (no index out of bounds)
            hashResult = (31 * hashResult + token.charAt(i)) % TABLESIZE;
        }
        return hashResult;
    }

    protected long getHashVerify(String token) {
        long hashResult = 0;
        for (int i = 0; i < token.length(); i++) {
            // mod TABLESIZE to wrap our limit space (no index out of bounds)
            hashResult = (37 * hashResult + token.charAt(i)) % TABLESIZE;
        }
        return hashResult;
    }

    protected boolean isColliding(long ptr, long verify) {
        var entry = readEntry(ptr);
        var colliding = verify != entry.verify && entry.verify != 0;
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
        int diff = 1;
        while (isColliding(hash * Entry.BYTE_SIZE, verify)) {
            hash = (hash + diff) % TABLESIZE;
            diff = diff * 2;
        }
        var entry = readEntry(hash * Entry.BYTE_SIZE);
        if (!entry.valid()) {
            return new PostingsList();
        }

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
     * Write the index to files.
     */
    public abstract void writeIndex();

    /**
     * Inserts this token in the main-memory hashtable.
     */
    public abstract void insert(String token, int docID, int offset);

    /**
     * Write index to file after indexing is done.
     */
    public abstract void cleanup();
}
