import os
import pandas as pd
from shingle import create_shingles
from jaccard import make_matrix
from pyspark.sql import SparkSession

spark = SparkSession.builder \
    .master('local[*]') \
    .appName('hw1') \
    .config("spark.driver.memory", "2g") \
    .getOrCreate()


def load_file(filepath):
    return spark.read.text(filepath, wholetext=True)


def load_documents(folder):
    filenames = [f for f in os.listdir(folder) if os.path.isfile(os.path.join(folder, f))]

    filedata = [load_file(folder + '/' + filename) for filename in filenames]

    combined = filedata[0]
    for i in range(1, len(filedata)):
        combined = combined.union(filedata[i])

    return combined


def main():
    # Value
    # bok 1
    # box 2

    # shingle,  hashed1 shingle     bok1, bok2, bok3
    # s1     ,  s1-hash1
    # s2     ,  s2-hash1
    # s3     ,  s3-hash1

    # Step 0: Load dataset

    print("===== Step 0: Load dataset =====")
    documents = load_documents('dataset/gutenberg')
    documents.show()

    # Step 1: shingle
    # A class Shingling that constructs k–shingles of a given length k (e.g., 10)
    # from a given document, computes a hash value for each unique shingle and represents
    # the document in the form of an ordered set of its hashed k-shingles.
    print("===== Step 1: Shingle =====")
    df = create_shingles(documents, 5)
    df.show()

    # Step 2: Compare sets
    # A class CompareSets computes the Jaccard similarity of two sets of integers –
    # two sets of hashed shingles.
    print("===== Step 2: Compare sets =====")
    make_matrix(df)

    #       doc1, doc2
    # doc1  1.0   0.6
    # doc2  0.6

# Step 3: MinHashing
    # A class MinHashing that builds a minHash signature (in the form of a vector or a set)
    # of a given length n from a given set of integers (a set of hashed shingles).

    # Step 4: CompareSignatures
    # A class CompareSignatures estimates the similarity of two integer vectors – minhash signatures –
    # as a fraction of components in which they agree.

    # Step 5: LSH
    # (Optional task for extra 2 bonus points) A class LSH that implements the LSH technique:
    # given a collection of minhash signatures (integer vectors) and a similarity threshold t,
    # the LSH class (using banding and hashing) finds candidate pairs of signatures agreeing on at least
    # a fraction t of their components.


if __name__ == '__main__':
    main()
