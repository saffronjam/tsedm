/*
 *   This file is part of the computer assignment for the
 *   Information Retrieval course at KTH.
 *
 *   Dmytro Kalpakchi, 2018
 */

package ir;


import java.io.Serializable;

public class KGramPostingsEntry implements Serializable {
    int tokenID;

    public KGramPostingsEntry(int tokenID) {
        this.tokenID = tokenID;
    }

    public KGramPostingsEntry(KGramPostingsEntry other) {
        this.tokenID = other.tokenID;
    }

    public String toString() {
        return tokenID + "";
    }
}
