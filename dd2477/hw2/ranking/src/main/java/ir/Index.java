/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Johan Boye, 2017
 */

package ir;

import java.util.HashMap;

/**
 * Defines some common data structures and methods that all types of
 * index should implement.
 */
public interface Index {

    /**
     * Mapping from document identifiers to document names.
     */
    HashMap<Integer, String> docNames = new HashMap<>();

    /**
     * Mapping from document identifier to document length.
     */
    HashMap<Integer, Integer> docLengths = new HashMap<>();

    HashMap<Integer, Double> docLengthsEuclidean = new HashMap<>();

    /**
     * Inserts a token into the index.
     */
    void insert(String token, int docID, int offset);

    /**
     * Returns the postings for a given term.
     */
    PostingsList getPostings(String token);

    /**
     * This method is called on exit.
     */
    void cleanup();

}

