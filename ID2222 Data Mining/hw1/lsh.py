def create_candidate_pairs(minhash_signatures, similarity_threshold):
    k = len(minhash_signatures[0])
    rows = 20
    band_size = k / rows

    # Dict<list, col>

    candiate_dict = {}


    # Document 1 signature
    doc1_signature = [1, 2, 3, 4, 5, 6]
    candiate_dict[doc1_signature[0:2]] = 'doc1'
    candiate_dict[doc1_signature[2:4]] = 'doc1'
    candiate_dict[doc1_signature[4:6]] = 'doc1'
    # ... k times

    # Document 2 signature
    doc2_signature = [1, 2, 9, 2, 5, 6]
    candiate_dict[doc2_signature[0:2]].append()
    candiate_dict[doc2_signature[2:4]] = 'doc2'
    candiate_dict[doc2_signature[4:6]] = 'doc3'
    # ... k times

    # 1   1
    # 2   2
    # --------
    # 5   3
    # 8   4

    pass
