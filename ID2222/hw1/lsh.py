import itertools
from typing import Dict, List


def hash_band(band):
    return ' '.join([str(elem) for elem in band])


def get_bands(signature, band_size):
    result = []
    for i in range(0, len(signature) - band_size, band_size):
        result.append(signature[i:i + band_size])
    return result


def create_candidate_pairs(minhash_signatures: Dict[str, List[int]], similarity_threshold):
    k = len(minhash_signatures['0'])
    rows = 30
    band_size = int(k / rows)
    candidate_dict = {}

    for document_id, signature in minhash_signatures.items():
        bands = get_bands(signature, band_size)
        for band in bands:
            hashed = hash_band(band)

            if hashed not in candidate_dict.keys():
                candidate_dict[hashed] = set()

            candidate_dict[hashed].add(document_id)

    def filter_fn(candidate_set):
        return len(candidate_set) > 1

    candidate_buckets = list(filter(filter_fn, candidate_dict.values()))

    permutations = []
    for bucket in candidate_buckets:
        tuples = list(itertools.permutations(bucket, 2))
        sorted_tuples = []
        for t in tuples:
            sorted_tuples.append(tuple(sorted(t)))
        permutations += sorted_tuples

    permutations = list(set(permutations))

    return permutations
