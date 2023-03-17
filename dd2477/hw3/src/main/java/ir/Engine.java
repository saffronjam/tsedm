/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Johan Boye, 2017
 */

package ir;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/**
 * This is the main class for the search engine.
 */
public class Engine {

    /**
     * The inverted index.
     */

    // Index index = new PersistentScalableHashedIndex();
    // Index index = new PersistentNonScalableHashedIndex();
    // Index index = new PersistentHashedIndex();
    Index index = new HashedIndex();

    /**
     * The indexer creating the search index.
     */
    Indexer indexer;

    /**
     * K-gram index
     */
    KGramIndex kgIndex = new KGramIndex(2);

    /**
     * The searcher used to search the index.
     */
    Searcher searcher;

    /**
     * Spell checker
     */
    SpellChecker speller;

    /**
     * The engine GUI.
     */
    SearchGUI gui;

    /**
     * Directories that should be indexed.
     */
    ArrayList<String> dirNames = new ArrayList<String>();

    /**
     * Lock to prevent simultaneous access to the index.
     */
    Object indexLock = new Object();

    /**
     * The patterns matching non-standard words (e-mail addresses, etc.)
     */
    String patterns_file = null;

    /**
     * The file containing the logo.
     */
    String pic_file = "";

    /**
     * The file containing the pageranks.
     */
    String rank_file = "";

    /**
     * For persistent indexes, we might not need to do any indexing.
     */
    boolean is_indexing = true;

    /* ----------------------------------------------- */

    HashMap<String, Double> idfs = new HashMap<>();
    HashMap<Integer, HashMap<String, Double>> tfIdfs = new HashMap<>();

    /**
     * Constructor.
     * Indexes all chosen directories and files
     */
    public Engine(String[] args) {
        decodeArgs(args);
        indexer = new Indexer(index, kgIndex, patterns_file);
        searcher = new Searcher(index, kgIndex);
        gui = new SearchGUI(this);
        gui.init();
        /*
         * Calls the indexer to index the chosen directory structure.
         * Access to the index is synchronized since we don't want to
         * search at the same time we're indexing new files (this might
         * corrupt the index).
         */
        if (is_indexing) {
            synchronized (indexLock) {
                gui.displayInfoText("Indexing, please wait...");
                long startTime = System.currentTimeMillis();
                for (var dirName : dirNames) {
                    File dokDir = new File(dirName);
                    indexer.processFiles(dokDir, is_indexing);
                }

                // convert tf-vector to tf-idf vector
                int i = 0;
                for (var counter : indexer.tokenCounters.values()) {
                    if (i % 1000 == 0) {
                        System.err.println("Calculated idf for " + i + " files");
                    }
                    i++;

                    for (var token : counter.keySet()) {
                        calculateIdf(token);
                    }
                }

                i = 0;
                for (var docTokenCounters : indexer.tokenCounters.entrySet()) {
                    if (i % 1000 == 0) {
                        System.err.println("Converted tf to tfIdf for " + i + " files");
                    }
                    i++;
                    var docId = docTokenCounters.getKey();

                    tfIdfs.put(docId, new HashMap<>());
                    var list = tfIdfs.get(docId);

                    for (var tokenCounter : docTokenCounters.getValue().entrySet()) {
                        var token = tokenCounter.getKey();
                        var tf = (double) tokenCounter.getValue();
                        var idf = idfs.get(token);
                        list.put(token, tf * idf);
                    }
                }

                // calculate euclidean lengths
                i = 0;
                for (var entry : tfIdfs.entrySet()) {
                    if (i % 1000 == 0) {
                        System.err.println("Calculated euclidean distance for " + i + " files");
                    }
                    i++;

                    var docId = entry.getKey();

                    var sumSq = 0.0;
                    for (var tokenTfIdfs : entry.getValue().entrySet()) {
                        sumSq += Math.pow(tokenTfIdfs.getValue(), 2.0);
                    }

                    var result = Math.sqrt(sumSq);
                    Index.docLengthsEuclidean.put(docId, result);
                }

                // save euclidean lengths to disk
                writeEuclideanData();

                long elapsedTime = System.currentTimeMillis() - startTime;
                gui.displayInfoText(String.format("Indexing done in %.1f seconds.", elapsedTime / 1000.0));
                index.cleanup();

                var veList = kgIndex.getPostings("ve");
                var thList = kgIndex.getPostings("th");
                var heList = kgIndex.getPostings("he");

                var thHeList = kgIndex.intersect(thList, heList);

                System.out.println("ve: " + veList.size());

                System.out.println("th: " + thList.size());
                System.out.println("he: " + heList.size());
                System.out.println("th he: " + thHeList.size());

            }
        } else {
            gui.displayInfoText("Index is loaded from disk");

            // load euclidean length from disk
            parseEuclideanData();
        }
    }

    private void calculateIdf(String token) {
        if (idfs.containsKey(token)) {
            return;
        }

        var df = indexer.dfCounters.get(token);

        var idf = Math.log((double) Index.docNames.size() / (double) df.size());

        idfs.put(token, idf);
    }

    void writeEuclideanData() {
        var builder = new StringBuilder();
        for (var entry : Index.docLengthsEuclidean.entrySet()) {
            builder.append(entry.getKey()).append(';').append(entry.getValue()).append('\n');
        }

        try (var writer = new BufferedWriter(new FileWriter("grade-c/euclideanDistances"))) {
            writer.write(builder.toString());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    void parseEuclideanData() {
        try (var reader = new BufferedReader(new FileReader("grade-c/euclideanDistances"))) {

            var line = reader.readLine();
            while (line != null) {
                var split = line.split(";");
                Index.docLengthsEuclidean.put(Integer.parseInt(split[0]), Double.parseDouble(split[1]));
                line = reader.readLine();
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /* ----------------------------------------------- */

    /**
     * Decodes the command line arguments.
     */
    private void decodeArgs(String[] args) {

        System.out.println(Arrays.toString(args));

        int i = 0;
        // int j = 0;
        while (i < args.length) {
            if ("-d".equals(args[i])) {
                i++;
                if (i < args.length) {
                    dirNames.add(args[i++]);
                }
            } else if ("-p".equals(args[i])) {
                i++;
                if (i < args.length) {
                    patterns_file = args[i++];
                }
            } else if ("-l".equals(args[i])) {
                i++;
                if (i < args.length) {
                    pic_file = args[i++];
                }
            } else if ("-r".equals(args[i])) {
                i++;
                if (i < args.length) {
                    rank_file = args[i++];
                }
            } else if ("-ni".equals(args[i])) {
                i++;
                is_indexing = false;
            } else {
                System.err.println("Unknown option: " + args[i]);
                break;
            }
        }
    }

    /* ----------------------------------------------- */

    public static void main(String[] args) {
        Engine e = new Engine(args);
    }

}
