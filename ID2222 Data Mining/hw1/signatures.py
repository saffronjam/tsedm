from utils import print_heatmap, print_table
from typing import Dict, Tuple, List


def compare_signatures(signatures: Dict):
    table = []

    for i, signature in enumerate(signatures.values()):
        k = len(signature)
        table.append([])
        for other_signature in signatures.values():
            # shared = 0
            # total = len(signature)
            # for value in signature:
            #     for other_value in other_signature:
            #         if value == other_value:
            #             shared += 1
            # table[i].append(shared / total)

            commonalities = len(set(signature).intersection(other_signature))
            table[i].append(commonalities / k)

    print_table(table)
    print_heatmap(table, 'Minhash')


def compare_signatures_by_pair(signature_pairs: List[Tuple[str]], signatures: Dict):
    table = []

    k = len(signatures['0'])
    size = len(signatures)

    for i in range(size):
        table.append([])
        for j in range(size):
            if i == j:
                table[i].append(1.0)
            else:
                table[i].append(0.0)

    for pair in signature_pairs:
        # Calculate similarity
        document_id1 = pair[0]
        document_id2 = pair[1]

        signature1 = signatures[document_id1]
        signature2 = signatures[document_id2]

        commonalities = len(set(signature1).intersection(signature2))

        # Add to heatmap
        i = int(document_id1)
        j = int(document_id2)
        table[i][j] = (commonalities / k)

    print_table(table)
    print_heatmap(table, 'LSH candidate pairs')
