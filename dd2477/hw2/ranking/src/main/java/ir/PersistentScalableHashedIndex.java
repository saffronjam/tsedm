/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Johan Boye, KTH, 2018
 */

package ir;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;

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
public class PersistentScalableHashedIndex extends PersistentHashedIndex {

    // guardian big
    public static final long INSERT_THRESHOLD = 8500000;

    // guardian small
    // public static final long INSERT_THRESHOLD = 600000;

    // davis big
    // public static final long INSERT_THRESHOLD = 1500000;

    // davis small
    // public static final long INSERT_THRESHOLD = 200000;

    public static final String BASE_DIR = "grade-a/";

    private int insertCounter = 0;
    private int insertBatchNumber = 0;

    private boolean verifyMerges = false;
    private boolean onlyVerify = false;

    private ArrayList<String> readyForMerge = new ArrayList<String>();

    public PersistentScalableHashedIndex() {
        super(BASE_DIR);

        // load if we already have an index
        var intermediateDirs = new File(BASE_DIR + "intermediate/").list();
        if (intermediateDirs != null && intermediateDirs.length == 1) {
            try {
                setIndexDirectory(BASE_DIR + "intermediate/" + intermediateDirs[0] + "/");
                readDocInfo(BASE_DIR + "intermediate/" + intermediateDirs[0] + "/");
            } catch (IOException e) {
                e.printStackTrace();
            }
            System.err.println("done!");
        }
    }

    @Override
    public void writeIndex() {
        int collisions = 0;
        try {
            // Write the 'docNames' and 'docLengths' hash maps to a file
            writeDocInfo(getIntermediateDirectoryName());

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
                long hash = getHashLocation(token);
                var findResult = getFirstFreeDictSpace(hash * Entry.BYTE_SIZE);
                collisions += findResult.collisions;

                // step 4: write to dictionary with ptr from step 2
                var entry = new Entry(dataPtr, bytesWritten);

                writeEntry(entry, findResult.ptr);

                dataPtr += bytesWritten;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.err.println(collisions + " collisions.");
    }

    private String getIntermediateDirectoryName() {
        var name = BASE_DIR + "intermediate/" + String.valueOf(insertBatchNumber) + "/";
        return name;
    }

    @Override
    public void insert(String token, int docID, int offset) {
        if (insertCounter > INSERT_THRESHOLD) {
            cleanup();
            insertBatchNumber++;
            insertCounter = 0;
        }

        var list = index.get(token);
        if (list == null) {
            list = new PostingsList(token);
        }
        list.add(docID, 0, offset);

        index.put(token, list);

        insertCounter++;
    }

    @Override
    public void cleanup() {
        System.err.println(index.keySet().size() + " unique words");
        System.err.print("Writing index to disk...");

        synchronized (readyForMerge) {
            var directory = getIntermediateDirectoryName();
            setIndexDirectory(directory);
            writeIndex();
            closeIndexDirectory();
            index.clear();
            docNames.clear();
            docLengths.clear();

            readyForMerge.add(directory);

            if (readyForMerge.size() > 1) {
                var dir1 = readyForMerge.get(0);
                var dir2 = readyForMerge.get(1);

                readyForMerge.remove(0);
                readyForMerge.remove(0);

                var outDir = getIntermediateOutDir(dir1, dir2);

                deployWorker(dir1, dir2, outDir);
            }
        }
    }

    interface WorkerHandler {
        public void onFinish();
    }

    private String getIntermediateOutDir(String dir1, String dir2) {
        var outDirName = Paths.get(dir1).getFileName() + "-" + Paths.get(dir2).getFileName() + "/";
        var outDir = BASE_DIR + "intermediate/" + outDirName;
        return outDir;
    }

    private void deployWorker(String dir1, String dir2, String outDir) {
        var worker = new Thread(() -> mergerWorker(dir1, dir2, outDir, () -> {
            synchronized (readyForMerge) {
                System.out.println("worker finished with output: " + outDir);
                readyForMerge.add(outDir);

                // check if this was the final merge
                var intermediateDirs = new File(BASE_DIR + "intermediate/").list();
                if (intermediateDirs.length == 1) {
                    try {
                        setIndexDirectory(outDir);
                        readDocInfo(outDir);
                        System.err.println("done!");
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }

                if (readyForMerge.size() > 1) {
                    var dir1Next = readyForMerge.get(0);
                    var dir2Next = readyForMerge.get(1);

                    readyForMerge.remove(0);
                    readyForMerge.remove(0);

                    var outDirNext = getIntermediateOutDir(dir1Next, dir2Next);

                    deployWorker(dir1Next, dir2Next, outDirNext);
                }
            }
        }));

        System.out.println("starting new worker, outputs to: " + outDir);
        worker.start();
    }

    private void mergerWorker(String dir1, String dir2, String outDir, WorkerHandler callback) {
        var dictionaryFname1 = dir1 + DICTIONARY_FNAME;
        var dataFname1 = dir1 + DATA_FNAME;
        var docInfoFname1 = dir1 + DOCINFO_FNAME;

        var dictionaryFname2 = dir2 + DICTIONARY_FNAME;
        var dataFname2 = dir2 + DATA_FNAME;
        var docInfoFname2 = dir2 + DOCINFO_FNAME;

        var dictionaryFnameOut = outDir + DICTIONARY_FNAME;
        var dataFnameOut = outDir + DATA_FNAME;
        var docInfoFnameOut = outDir + DOCINFO_FNAME;

        try {
            // step 1: create output files
            createDirsAndFile(dictionaryFnameOut);
            createDirsAndFile(dataFnameOut);
            createDirsAndFile(docInfoFnameOut);

            // step 2: open files
            var dictionary1 = new RandomAccessFile(dictionaryFname1, "rw");
            var data1 = new RandomAccessFile(dataFname1, "rw");
            var docInfo1 = new RandomAccessFile(docInfoFname1, "rw");

            var dictionary2 = new RandomAccessFile(dictionaryFname2, "rw");
            var data2 = new RandomAccessFile(dataFname2, "rw");
            var docInfo2 = new RandomAccessFile(docInfoFname2, "rw");

            var dictionaryOut = new RandomAccessFile(dictionaryFnameOut, "rw");
            var dataOut = new RandomAccessFile(dataFnameOut, "rw");
            var docInfoOut = new RandomAccessFile(docInfoFnameOut, "rw");

            var end = Entry.BYTE_SIZE * TABLESIZE;
            dictionaryOut.seek(end);
            dictionaryOut.writeByte(0);

            if (!onlyVerify) {
                // step 3: merge on a per-PostingsList basis
                var outPtr = 0;
                // read all entries from first file, all these will be unique
                for (long dictPtr1 = 0; dictPtr1 < TABLESIZE * Entry.BYTE_SIZE; dictPtr1 += Entry.BYTE_SIZE) {
                    var entry1 = readEntry(dictionary1, dictPtr1);
                    if (!entry1.valid()) {
                        continue;
                    }
                    var token1 = readToken(data1, entry1.ptr);
                    var startPtr = getHashLocation(token1) * Entry.BYTE_SIZE;

                    // look up matching entry in the other dictionary, since the same hash function
                    // was used, it should be able to find it, though not necessarily in the same
                    // place due to collisions
                    var entry2 = getEntryWithToken(dictionary2, data2, startPtr, token1);

                    if (entry2 == null) {
                        // entry in first dictionary was not found in other dictionary,
                        // so no merging is required (only moving from dir1 -> outDir directly)
                        var rawData = readData(data1, entry1.ptr, entry1.size);
                        var bytesWritten = writeData(dataOut, rawData, outPtr);

                        var outEntry = new Entry(outPtr, bytesWritten);
                        writeEntry(dictionaryOut, outEntry, dictPtr1);

                        outPtr += bytesWritten;

                        continue;
                    }

                    // otherwise we need to merge PostingsLists, since they both contain an entry
                    // for a word
                    var rawData1 = readData(data1, entry1.ptr, entry1.size);
                    var rawData2 = readData(data2, entry2.ptr, entry2.size);

                    var postingsList1 = parsePostingsList(rawData1);
                    var postingsList2 = parsePostingsList(rawData2);
                    var postingsListMerged = mergePostingsList(postingsList1, postingsList2);

                    var rawData = marshallPostingsList(postingsListMerged);
                    var bytesWritten = writeData(dataOut, rawData, outPtr);

                    var outEntry = new Entry(outPtr, bytesWritten);
                    writeEntry(dictionaryOut, outEntry, dictPtr1);

                    outPtr += bytesWritten;
                }

                // read all entries from second file, might not be all unique
                for (long dictPtr2 = 0; dictPtr2 < TABLESIZE * Entry.BYTE_SIZE; dictPtr2 += Entry.BYTE_SIZE) {
                    var entry2 = readEntry(dictionary2, dictPtr2);
                    if (!entry2.valid()) {
                        continue;
                    }
                    var token2 = readToken(data2, entry2.ptr);
                    var startPtr = getHashLocation(token2) * Entry.BYTE_SIZE;

                    // now we only need to add any entry that is valid for entry2, but not entry1
                    // shared entries were already added in the previous step
                    var entry1 = getEntryWithToken(dictionary1, data1, startPtr, token2);

                    // add only if entry2 is unique, otherwise it is already added in the previous
                    // loop
                    if (entry1 == null) {
                        var rawData = readData(data2, entry2.ptr, entry2.size);
                        var bytesWritten = writeData(dataOut, rawData, outPtr);

                        // finally, we need to find a free place for this entry in out
                        var findResult = getFirstFreeDictSpace(dictionaryOut, startPtr);
                        var outEntry = new Entry(outPtr, bytesWritten);
                        writeEntry(dictionaryOut, outEntry, findResult.ptr);

                        outPtr += bytesWritten;
                    }

                }

                // step 5: merge docInfos
                docInfo1.seek(0);
                docInfo2.seek(0);
                docInfoOut.seek(0);

                var line1 = docInfo1.readLine();
                var line2 = docInfo2.readLine();

                var lf = System.lineSeparator().getBytes();

                while (true) {
                    if (line1 == null && line2 == null) {
                        break;
                    }

                    if (line1 == null) {
                        docInfoOut.write(line2.getBytes());
                        docInfoOut.write(lf);

                        line2 = docInfo2.readLine();
                    } else if (line2 == null) {
                        docInfoOut.write(line1.getBytes());
                        docInfoOut.write(lf);

                        line1 = docInfo1.readLine();
                    } else {
                        var docId1 = Integer.valueOf(line1.split(";", 2)[0]);
                        var docId2 = Integer.valueOf(line2.split(";", 2)[0]);

                        if (docId1 == docId2) {
                            docInfoOut.write(line1.getBytes());
                            docInfoOut.write(lf);

                            line1 = docInfo1.readLine();
                            line2 = docInfo2.readLine();
                        } else if (docId1 < docId2) {
                            docInfoOut.write(line1.getBytes());
                            docInfoOut.write(lf);

                            line1 = docInfo1.readLine();
                        } else {
                            docInfoOut.write(line2.getBytes());
                            docInfoOut.write(lf);

                            line2 = docInfo2.readLine();
                        }
                    }

                }
            }

            if (verifyMerges) {
                verify(dictionary1, dictionary2, dictionaryOut, data1, data2, dataOut);
            }

            // step 5: delete folder1 and folder2 since they are merged
            dictionary1.close();
            data1.close();
            docInfo1.close();

            dictionary2.close();
            data2.close();
            docInfo2.close();

            dictionaryOut.close();
            dataOut.close();
            docInfoOut.close();

            deleteDir(dir1);
            deleteDir(dir2);
        } catch (IOException io) {
            io.printStackTrace();
        }

        callback.onFinish();
    }

    private void setIndexDirectory(String folder) {
        try {
            var dictionaryFilename = folder + DICTIONARY_FNAME;
            var dataFilename = folder + DATA_FNAME;
            var docInfoFilename = folder + DOCINFO_FNAME;

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
    }

    private void closeIndexDirectory() {
        try {
            dictionaryFile.close();
            dataFile.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void verify(RandomAccessFile dictionary1, RandomAccessFile dictionary2, RandomAccessFile dictionaryOut,
                        RandomAccessFile data1, RandomAccessFile data2, RandomAccessFile dataOut) {

        var uniqueTokens = new HashSet<String>();
        var uniqueTokenMerged = new HashSet<String>();

        for (long dictPtr = 0; dictPtr < TABLESIZE * Entry.BYTE_SIZE; dictPtr += Entry.BYTE_SIZE) {
            var entry1 = readEntry(dictionary1, dictPtr);
            if (entry1.valid()) {
                var token1 = readToken(data1, entry1.ptr);
                uniqueTokens.add(token1);

                var startPointer = getHashLocation(token1) * Entry.BYTE_SIZE;
                var rawData1 = readData(data1, entry1.ptr, entry1.size);
                var postingsList1 = parsePostingsList(rawData1);

                var entryOut = getEntryWithToken(dictionaryOut, dataOut, startPointer, token1);
                var postingsListOut = entryOut != null
                        ? parsePostingsList(readData(dataOut, entryOut.ptr, entryOut.size))
                        : new PostingsList("");

                for (int i = 0; i < postingsList1.size(); i++) {
                    var postingsEntry1 = postingsList1.get(i);
                    var postingsEntryOut = postingsListOut.getByDocId(postingsEntry1.docID);

                    if (postingsEntryOut == null) {
                        System.out.println("invalid state docId");
                    }

                    for (int j = 0; j < postingsEntry1.offsets.size(); j++) {
                        if (!postingsEntryOut.offsets.contains(postingsEntry1.offsets.get(j))) {
                            System.out.println("invalid state merged offsets");
                        }
                    }
                }
            }

            var entry2 = readEntry(dictionary2, dictPtr);
            if (entry2.valid()) {
                var token2 = readToken(data2, entry2.ptr);
                uniqueTokens.add(token2);

                var startPointer = getHashLocation(token2) * Entry.BYTE_SIZE;
                var rawData2 = readData(data2, entry2.ptr, entry2.size);
                var postingsList2 = parsePostingsList(rawData2);

                var entryOut = getEntryWithToken(dictionaryOut, dataOut, startPointer, token2);
                var postingsListOut = entryOut != null
                        ? parsePostingsList(readData(dataOut, entryOut.ptr, entryOut.size))
                        : new PostingsList("");

                for (int i = 0; i < postingsList2.size(); i++) {
                    var postingsEntry2 = postingsList2.get(i);
                    var postingsEntryOut = postingsListOut.getByDocId(postingsEntry2.docID);

                    if (postingsEntryOut == null) {
                        System.out.println("invalid state docId");
                    }

                    for (int j = 0; j < postingsEntry2.offsets.size(); j++) {
                        if (!postingsEntryOut.offsets.contains(postingsEntry2.offsets.get(j))) {
                            System.out.println("invalid state merged offsets");
                        }
                    }
                }

            }
        }

        for (long dictPtr = 0; dictPtr < TABLESIZE * Entry.BYTE_SIZE; dictPtr += Entry.BYTE_SIZE) {
            var entryOut = readEntry(dictionaryOut, dictPtr);

            if (!entryOut.valid()) {
                var entry1 = readEntry(dictionary1, dictPtr);
                var entry2 = readEntry(dictionary2, dictPtr);
                if (entry1.valid() && entry2.valid()) {
                    System.out.println("invalid state validity");
                }
            } else {
                var token = readToken(dataOut, entryOut.ptr);
                uniqueTokenMerged.add(token);

                var startPointer = getHashLocation(token) * Entry.BYTE_SIZE;
                var rawData = readData(dataOut, entryOut.ptr, entryOut.size);
                var postingsList = parsePostingsList(rawData);

                var entry1 = getEntryWithToken(dictionary1, data1, startPointer, token);
                var postingsList1 = entry1 != null ? parsePostingsList(readData(data1, entry1.ptr, entry1.size))
                        : new PostingsList("");

                var entry2 = getEntryWithToken(dictionary2, data2, startPointer, token);
                var postingsList2 = entry2 != null ? parsePostingsList(readData(data2, entry2.ptr, entry2.size))
                        : new PostingsList("");

                for (int i = 0; i < postingsList.size(); i++) {
                    var postingsEntry = postingsList.get(i);

                    var postingsEntry1 = postingsList1.getByDocId(postingsEntry.docID);
                    var postingsEntry2 = postingsList2.getByDocId(postingsEntry.docID);

                    if (postingsEntry1 == null && postingsEntry2 == null) {
                        System.out.println("invalid state docId");
                    }

                    var allOffsets = new ArrayList<Integer>();
                    if (postingsEntry1 != null) {
                        allOffsets.addAll(postingsEntry1.offsets);
                    }
                    if (postingsEntry2 != null) {
                        allOffsets.addAll(postingsEntry2.offsets);
                    }

                    if (allOffsets.size() != postingsEntry.offsets.size()) {
                        System.out.println("invalid state offset size");
                    }

                    for (int j = 0; j < allOffsets.size(); j++) {
                        if (!allOffsets.get(j).equals(postingsEntry.offsets.get(j))) {
                            System.out.println("invalid state offset value");
                        }
                    }
                }
            }
        }

        var intersect = new HashSet<String>(uniqueTokens);
        intersect.retainAll(uniqueTokenMerged);
        if (uniqueTokens.size() != uniqueTokenMerged.size() || intersect.size() != uniqueTokens.size()) {
            System.out.println("invalid state unique words");
        }

        for (var token : uniqueTokens) {
            var postings1 = getPostings(dictionary1, data1, token);
            var postings2 = getPostings(dictionary2, data2, token);
            var postingsOut = getPostings(dictionaryOut, dataOut, token);

            var keyMerged = new HashSet<Integer>(postings1.getMap().keySet());
            keyMerged.addAll(postings2.getMap().keySet());

            var matchesMerged = keyMerged.size();
            var matchesOut = postingsOut.size();

            if (matchesMerged != matchesOut) {
                System.out.println("invalid state matches");
            }
        }

        System.out.println("verification complete");
    }
}
