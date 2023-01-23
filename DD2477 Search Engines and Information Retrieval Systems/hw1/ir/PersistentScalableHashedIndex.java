/*  
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 * 
 *   Johan Boye, KTH, 2018
 */

package ir;

import java.io.*;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Function;

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

    public static final int INSERT_THRESHOLD = 50000;

    public static final String BASE_DIR = "grade-a/";

    private int insertCounter = 0;
    private int insertBatchNumber = 0;

    private ArrayList<String> readyForMerge = new ArrayList<String>();

    public PersistentScalableHashedIndex() {
        super(BASE_DIR);
    }

    // ==================================================================

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

    // ==================================================================

    @Override
    public void writeIndex() {
        int collisions = 0;
        try {
            // Write the 'docNames' and 'docLengths' hash maps to a file
            // writeDocInfo();

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
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.err.println(collisions + " collisions.");
    }

    @Override
    public void insert(String token, int docID, int offset) {
        if (insertCounter > INSERT_THRESHOLD) {
            // write index to file then post job to worker
            // wait if worker is currently merging
            var folder = BASE_DIR + "intermediate/" + String.valueOf(insertBatchNumber) + "/";
            setIndexDirectory(folder);
            cleanup();

            readyForMerge.add(folder);

            insertBatchNumber++;
            insertCounter = 0;

            if (readyForMerge.size() > 1) {
                var dir1 = readyForMerge.get(0);
                var dir2 = readyForMerge.get(1);

                readyForMerge.remove(0);
                readyForMerge.remove(0);

                var outDirName = Paths.get(dir1).getFileName() + "-" + Paths.get(dir2).getFileName() + "/";
                var outDir = BASE_DIR + "intermediate/" + outDirName;

                // TEMPORARY if case, only send out one worker
                if (insertBatchNumber == 2) {
                    var worker = new Thread(() -> {
                        mergerWorker(dir1, dir2, outDir, () -> {

                        });
                    });

                    worker.start();
                }
            }

        }

        var list = index.get(token);
        if (list == null) {
            list = new PostingsList();
        }

        list.add(docID, 0, offset);

        index.put(token, list);

        insertCounter++;
    }

    @Override
    public void cleanup() {
        System.err.println(index.keySet().size() + " unique words");
        System.err.print("Writing index to disk...");
        writeIndex();

        System.err.print("Resetting index and filepointsers");
        index = new HashMap<>();
        try {
            dictionaryFile.close();
            dataFile.close();
        } catch (IOException io) {
            io.printStackTrace();
        }

        System.err.println("done!");
    }

    interface WorkerHandler {
        public void onFinish();
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

            // step 3: merge on a per-PostingsList basis

            var outPtr = 0;
            // read all entries from first file, all these will be unique
            for (long dictPtr1 = 0; dictPtr1 < TABLESIZE * Entry.BYTE_SIZE; dictPtr1 += Entry.BYTE_SIZE) {
                var entry1 = readEntry(dictionary1, dictPtr1);
                if (!entry1.valid()) {
                    continue;
                }

                // look up matching entry in the other dictionary, since the same hash function
                // was used, it should be able to find it, though not necessarily in the same
                // place due to collisions
                var dictPtr2 = dictPtr1;
                int diff = 1;
                var entry2 = readEntry(dictionary2, dictPtr2);
                while (entry1.verify != entry2.verify && entry2.valid()) {
                    dictPtr2 = (dictPtr2 + diff) % TABLESIZE;
                    entry2 = readEntry(dictionary2, dictPtr2);
                    diff = diff * 2;
                }

                if (!entry2.valid()) {
                    // entry in first dictionary was not found in other dictionary,
                    // so no merging is required (only moving from dir1 -> outDir directly)
                    var rawData = readData(data1, entry1.ptr, entry1.size);
                    var bytesWritten = writeData(dataOut, rawData, outPtr);

                    var outEntry = new Entry();
                    outEntry.ptr = outPtr;
                    outEntry.verify = entry1.verify;
                    outEntry.size = bytesWritten;

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

                var outEntry = new Entry();
                outEntry.ptr = outPtr;
                outEntry.verify = entry1.verify;
                outEntry.size = bytesWritten;

                writeEntry(dictionaryOut, outEntry, dictPtr1);

                outPtr += bytesWritten;
            }

            // read all entries from second file, might not be all unique
            for (long dictPtr2 = 0; dictPtr2 < TABLESIZE * Entry.BYTE_SIZE; dictPtr2 += Entry.BYTE_SIZE) {
                var entry2 = readEntry(dictionary2, dictPtr2);
                if (!entry2.valid()) {
                    continue;
                }

                // now we only need to add any entry that is valid for entry2, but not entry1
                // shared entries were already added in the previous step
                var dictPtr1 = dictPtr2;
                int diff = 1;
                var entry1 = readEntry(dictionary1, dictPtr1);
                while (entry2.verify != entry1.verify && entry1.valid()) {
                    dictPtr1 = (dictPtr2 + diff) % TABLESIZE;
                    entry1 = readEntry(dictionary1, dictPtr1);
                    diff = diff * 2;
                }

                // add only if entry2 is unique, otherwise it is already added in the previous
                // loop
                if (!entry1.valid()) {
                    // finally, we need to find a free place for this entry in out
                    var outPtrWrite = dictPtr2;
                    diff = 1;
                    var outEntryRead = readEntry(dictionaryOut, outPtrWrite);
                    while (outEntryRead.valid()) {
                        outPtrWrite = (dictPtr2 + diff) % TABLESIZE;
                        outEntryRead = readEntry(dictionaryOut, outPtrWrite);
                        diff = diff * 2;
                    }

                    var rawData = readData(data2, entry2.ptr, entry2.size);
                    var bytesWritten = writeData(dataOut, rawData, outEntryRead.ptr);

                    var outEntry = new Entry();
                    outEntry.ptr = outPtr;
                    outEntry.verify = entry2.verify;
                    outEntry.size = bytesWritten;

                    writeEntry(dictionaryOut, outEntry, outPtrWrite);

                    outPtr += bytesWritten;
                }

            }

            verify(dictionary1, dictionary2, dictionaryOut, data1, data2, dataOut);

            // step 4: delete folder1 and folder2 since they are merged
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

    private void verify(RandomAccessFile dictionary1, RandomAccessFile dictionary2, RandomAccessFile dictionaryOut,
            RandomAccessFile data1, RandomAccessFile data2, RandomAccessFile dataOut) {
        for (long dictPtr = 0; dictPtr < TABLESIZE * Entry.BYTE_SIZE; dictPtr += Entry.BYTE_SIZE) {
            var entryOut = readEntry(dictionaryOut, dictPtr);

            if (!entryOut.valid()) {
                var entry1 = readEntry(dictionary2, dictPtr);
                if (entry1.valid()) {
                    System.out.println("invalid state 1");
                }

                var entry2 = readEntry(dictionary2, dictPtr);
                if (entry2.valid()) {
                    System.out.println("invalid state 2");
                }
            } else {
                // TODO: docID 123 overwrite data and ptr = 0
                // verify should be 275500
                // find out why docID 123 overwrites and ptr = 0
                var rawData = readData(dataOut, entryOut.ptr, entryOut.size);
                var postingsList = parsePostingsList(rawData);

                var dictPtr1 = dictPtr;
                int diff = 1;
                var entry1 = readEntry(dictionaryOut, dictPtr1);
                while (entryOut.verify != entry1.verify && entry1.valid()) {
                    dictPtr1 = (dictPtr + diff) % TABLESIZE;
                    entry1 = readEntry(dictionaryOut, dictPtr1);
                    diff = diff * 2;
                }

                var postingsList1 = entry1.valid() ? parsePostingsList(readData(data1, entry1.ptr, entry1.size))
                        : new PostingsList();

                var dictPtr2 = dictPtr;
                diff = 1;
                var entry2 = readEntry(dictionaryOut, dictPtr2);
                while (entryOut.verify != entry2.verify && entry2.valid()) {
                    dictPtr2 = (dictPtr + diff) % TABLESIZE;
                    entry2 = readEntry(dictionaryOut, dictPtr2);
                    diff = diff * 2;
                }

                var postingsList2 = entry2.valid() ? parsePostingsList(readData(data2, entry2.ptr, entry2.size))
                        : new PostingsList();

                for (int i = 0; i < postingsList.size(); i++) {
                    var postingsEntry = postingsList.get(i);

                    var postingsEntry1 = postingsList1.getByDocId(postingsEntry.docID);
                    var postingsEntry2 = postingsList2.getByDocId(postingsEntry.docID);

                    if (postingsEntry1 == null && postingsEntry2 == null) {
                        System.out.println("invalid state docId");
                    }

                    var allOffsets = new ArrayList<Integer>();
                    allOffsets.addAll(postingsEntry1.offsets);
                    allOffsets.addAll(postingsEntry2.offsets);

                    if (allOffsets.size() != postingsEntry.offsets.size()) {
                        System.out.println("invalid state offset size");
                    }

                    for (int j = 0; j < allOffsets.size(); j++) {
                        if (allOffsets.get(j) != postingsEntry.offsets.get(j)) {
                            System.out.println("invalid state offset value");
                        }
                    }
                }
            }

        }
    }
}
