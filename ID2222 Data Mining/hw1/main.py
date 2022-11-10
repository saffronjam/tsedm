import os
from shingle import create_shingles
from jaccard import make_matrix
from pyspark.sql import SparkSession
from minhash import build_signature
from signatures import compare_signatures, compare_signatures_by_pair
from lsh import create_candidate_pairs

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
    # Step 0: Load dataset
    print("===== Step 0: Load dataset =====")
    documents = load_documents('dataset/gutenberg')
    documents.show()

    # Step 1: shingle
    # A class Shingling that constructs k–shingles of a given length k (e.g., 10)
    # from a given document, computes a hash value for each unique shingle and represents
    # the document in the form of an ordered set of its hashed k-shingles.
    print("===== Step 1: Shingle =====")
    characteristic, shingles = create_shingles(documents, 5)
    characteristic.show()

    # Step 2: Compare sets
    # A class CompareSets computes the Jaccard similarity of two sets of integers –
    # two sets of hashed shingles.
    print("===== Step 2: Compare sets =====")
    make_matrix(characteristic)

    # Step 3: MinHashing
    # A class MinHashing that builds a minHash signature (in the form of a vector or a set)
    # of a given length n from a given set of integers (a set of hashed shingles).
    print("===== Step 3: Build Signature =====")
    signatures = build_signature(shingles)
    print(signatures)

    # Step 4: CompareSignatures
    # A class CompareSignatures estimates the similarity of two integer vectors – minhash signatures –
    # as a fraction of components in which they agree.
    print("===== Step 4: Compare signatures =====")
    compare_signatures(signatures)

    # Step 5: LSH
    # (Optional task for extra 2 bonus points) A class LSH that implements the LSH technique:
    # given a collection of minhash signatures (integer vectors) and a similarity threshold t,
    # the LSH class (using banding and hashing) finds candidate pairs of signatures agreeing on at least
    # a fraction t of their components.
    print("===== Step 5: Find candidate pairs (LSH) =====")
    candidate_pairs = create_candidate_pairs(signatures, 0.4)
    compare_signatures_by_pair(candidate_pairs, signatures)


if __name__ == '__main__':
    main()
