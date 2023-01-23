/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, KTH, 2018
 */

package ir;

import java.io.*;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
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
    // public static final long TABLESIZE = 611953L;

    /** The dictionary hash table is stored in this file. */
    protected RandomAccessFile dictionaryFile;

    /** The data (the PostingsLists) are stored in this file. */
    protected RandomAccessFile dataFile;

    /** Pointer to the first free memory cell in the data file. */
    protected long free = 0L;

    /** The cache as a main-memory hash map. */
    protected HashMap<String, PostingsList> index = new HashMap<String, PostingsList>();

    // ===================================================================

    public static final boolean DELETE_ON_START = false;

    protected String BASE_DIR;

    /**
     * A helper class representing one entry in the dictionary hashtable.
     */
    public class Entry {
        public static final int BYTE_SIZE = 12;

        public long ptr;
        public int size;

        public Entry() {

        }

        public Entry(long ptr, int size) {
            this.ptr = ptr;
            this.size = size;
        }

        public boolean valid() {
            return !(ptr == 0 && size == 0);
        }
    }

    protected static void createDirsAndFile(String filename) throws IOException {
        var file = new File(filename);
        file.getParentFile().mkdirs();
        file.createNewFile();
    }

    protected static void deleteDir(String dir) throws IOException {
        Path directory = Paths.get(dir);

        if (Files.exists(directory)) {
            Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path path, BasicFileAttributes basicFileAttributes)
                        throws IOException {
                    Files.delete(path);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path directory, IOException ioException) throws IOException {
                    Files.delete(directory);
                    return FileVisitResult.CONTINUE;
                }
            });
        }
    }

    // ==================================================================

    /**
     * Constructor. Opens the dictionary file and the data file.
     * If these files don't exist, they will be created.
     */
    public PersistentHashedIndex(String baseDir) {
        this.BASE_DIR = baseDir;

    }

    /**
     * Writes data to the data file at a specified place.
     *
     * @return The number of bytes written.
     */
    int writeData(RandomAccessFile file, String dataString, long ptr) {
        try {
            file.seek(ptr);
            byte[] data = dataString.getBytes();
            file.write(data);
            return data.length;
        } catch (IOException e) {
            e.printStackTrace();
            return -1;
        }
    }

    /**
     * Writes data to the data file at a specified place.
     *
     * @return The number of bytes written.
     */
    int writeData(String dataString, long ptr) {
        return writeData(dataFile, dataString, ptr);
    }

    /**
     * Reads data from the data file
     */
    String readData(RandomAccessFile file, long ptr, int size) {
        try {
            file.seek(ptr);
            byte[] data = new byte[size];
            file.readFully(data);
            return new String(data);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Reads data from the data file
     */
    String readData(long ptr, int size) {
        return readData(dataFile, ptr, size);
    }

    /**
     * Reads data from the data file
     */
    String readToken(RandomAccessFile file, long ptr) {
        try {
            file.seek(ptr);

            var builder = new StringBuilder();

            char ch = (char) file.read();
            while (ch != ';') {
                builder.append(ch);
                ch = (char) file.read();
            }

            return builder.toString();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Reads data from the data file
     */
    String readToken(long ptr) {
        return readToken(dataFile, ptr);
    }

    /**
     * Writes the document names and document lengths to file.
     *
     * @throws IOException { exception_description }
     */
    protected abstract void writeDocInfo() throws IOException;

    /**
     * Reads the document names and document lengths from file, and
     * put them in the appropriate data structures.
     *
     * @throws IOException { exception_description }
     */
    protected abstract void readDocInfo() throws IOException;

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
    protected void writeEntry(RandomAccessFile file, Entry entry, long ptr) {
        try {
            file.seek(ptr);
            file.writeLong(entry.ptr);

            file.seek(ptr + 8);
            file.writeInt(entry.size);

        } catch (IOException e) {
            System.err.println("failed to write entry: " + e.getMessage());
            e.printStackTrace();
        }
    }

    protected void writeEntry(Entry entry, long ptr) {
        writeEntry(dictionaryFile, entry, ptr);
    }

    /**
     * Reads an entry from the dictionary file.
     *
     * @param ptr The place in the dictionary file where to start reading.
     */
    protected Entry readEntry(RandomAccessFile file, long ptr) {
        try {
            var entry = new Entry();

            file.seek(ptr);
            entry.ptr = file.readLong();

            file.seek(ptr + 8);
            entry.size = file.readInt();

            return entry;
        } catch (IOException e) {
            System.err.println("failed to read entry: " + e.getMessage());
            e.printStackTrace();
            return null;
        }
    }

    protected Entry readEntry(long ptr) {
        return readEntry(dictionaryFile, ptr);
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

    protected Entry getEntryWithToken(
            RandomAccessFile dictionaryFile,
            RandomAccessFile dataFile,
            long startPtr,
            String token) {

        int diff = 1;
        long readPtr = startPtr;

        var entry = readEntry(dictionaryFile, startPtr);
        var dataToken = "";
        if (entry.valid()) {
            dataToken = readToken(dataFile, entry.ptr);
        }

        while (!dataToken.equals(token) && entry.valid()) {
            readPtr = (readPtr + diff * Entry.BYTE_SIZE) % TABLESIZE;
            entry = readEntry(dictionaryFile, readPtr);
            dataToken = "";
            if (entry.valid()) {
                dataToken = readToken(dataFile, readPtr);
            }
            diff *= 2;
        }

        if (!entry.valid()) {
            return null;
        }

        return entry;
    }

    protected Entry getEntryWithToken(long startPtr, String token) {
        return getEntryWithToken(dictionaryFile, dataFile, startPtr, token);
    }

    protected long getFirstFreeDictSpace(RandomAccessFile dictionaryFile, long startPtr) {
        int diff = 1;
        long readPtr = startPtr;

        var entry = readEntry(dictionaryFile, startPtr);

        while (entry != null) {
            readPtr = (readPtr + diff * Entry.BYTE_SIZE) % TABLESIZE;
            entry = readEntry(dictionaryFile, readPtr);
            diff *= 2;
        }

        return readPtr;
    }

    protected long getFirstFreeDictSpace(long startPtr) {
        return getFirstFreeDictSpace(dictionaryFile, startPtr);
    }

    /**
     * Returns the postings for a specific term, or null
     * if the term is not in the index.
     */
    public PostingsList getPostings(String token) {
        // step 1: look up in dictionary to get ptr
        long hash = getHashLocation(token);

        var entry = getEntryWithToken((hash * Entry.BYTE_SIZE) % TABLESIZE, token);
        if (entry == null) {
            return new PostingsList("");
        }

        var dataPtr = entry.ptr;

        // step 2: load data at ptr
        var rawData = readData(dataPtr, entry.size);

        // step 3: parse data into PostingsList
        var postingsList = parsePostingsList(rawData);

        return postingsList;
    }

    protected PostingsList parsePostingsList(String rawData) {
        var intialSplit = rawData.split(";", 2);

        var token = intialSplit[0];
        var postingsListParts = intialSplit[1].split("\\|");

        var postingsList = new PostingsList(token);

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

    protected String marshallPostingsList(PostingsList postingsList) {
        var stringBuilder = new StringBuilder();

        stringBuilder
                .append(postingsList.getToken())
                .append(';');

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

        return stringData;
    }

    protected String getPostingsListToken(String rawData) {
        int delimIndex = rawData.indexOf(';');
        return rawData.substring(0, delimIndex);
    }

    protected PostingsList mergePostingsList(PostingsList postingsList1, PostingsList postingsList2) {
        var merged = new PostingsList(postingsList1.getToken());
        for (int i = 0; i < postingsList1.size(); i++) {
            var entry1 = postingsList1.get(i);
            var entry2 = postingsList2.getByDocId(entry1.docID);
            if (entry2 == null) {
                merged.add(entry1);
                continue;
            }

            var mergedEntry = mergePostingsEntry(entry1, entry2);

            merged.add(mergedEntry);
        }

        for (int i = 0; i < postingsList2.size(); i++) {
            var entry2 = postingsList2.get(i);
            if (merged.getByDocId(i) == null) {
                merged.add(entry2);
            }
        }

        return merged;
    }

    private PostingsEntry mergePostingsEntry(PostingsEntry postingsEntry1, PostingsEntry postingsEntry2) {
        var merged = new PostingsEntry(postingsEntry1.docID, postingsEntry1.score);
        merged.offsets.addAll(postingsEntry1.offsets);
        merged.offsets.addAll(postingsEntry2.offsets);
        return merged;
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
