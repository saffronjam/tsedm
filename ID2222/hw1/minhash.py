# https://mccormickml.com/2015/06/12/minhash-tutorial-with-python-code/
# k should be 100?
import random


def build_signature(shingled_documents):
    signatures = {}

    k = 100
    a = random.sample(range(1, 2 ** 28 - 1), k)
    b = random.sample(range(1, 2 ** 28 - 1), k)
    c = 4294967311

    for document_id, shingles in enumerate([data[0] for data in shingled_documents.select("shingles").collect()]):
        signature = []

        for hash_index in range(k):

            min_hash = 2 ** 32

            for shingle_id, shingle in enumerate(shingles):
                min_hash = min(min_hash, (a[hash_index] * shingle + b[hash_index]) % c)

            signature.append(min_hash)

        signatures[str(document_id)] = signature

    return signatures
