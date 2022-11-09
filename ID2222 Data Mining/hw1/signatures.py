from utils import print_heatmap, print_table
from typing import Dict


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
