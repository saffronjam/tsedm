from apriori import find_frequent_itemsets
from confidence import generate_associations

import pandas as pd


def load_document(filepath: str):
    with open(filepath, 'r') as file:
        lines = [line.rstrip() for line in file]
        return [set([eval(item) for item in line.split(' ')]) for line in lines]


def highest_item(baskets):
    largest = 0
    for basket in baskets:
        for item in basket:
            largest = max(item, largest)
    return largest


def check(kton, baskets):
    n = 0

    for basket in baskets:

        intersection = basket.intersection(kton)

        if len(intersection) == len(kton):
            n += 1
    print(f'itemset: {kton} had {n} occurences')


def main():
    support = 0.01
    confidence = 0.5

    print("==== FREQUENT ITEMSETS ====")

    print("===== Step 0: Load dataset-large =====")
    # dataset-small: 10 000 items
    # dataset-medium: 50 000 items
    # dataset-large: 100 000 items
    baskets = load_document('dataset-small')

    print(f' - support: {support}% ({round(support * len(baskets))} items) out of {len(baskets)}')

    # for kton in [{227, 390, 722}]:
    #     check(kton, baskets)

    print("===== Step 1: Define range =====")
    max_value = highest_item(baskets)
    print(f'Max value: {max_value}')

    print("===== Step 2: Find k-tons =====")
    itemsets, itemsets_per_basket = find_frequent_itemsets(baskets, max_value, support)

    for k, ktons in itemsets.items():
        print(f'{k}: ({len(ktons)}) {ktons}')

    print("===== Step 3: Find association rules =====")
    associations = generate_associations(baskets, itemsets_per_basket, max_value, confidence)

    sorted_ass = sorted(associations, key=lambda x: x['confidence'], reverse=True)

    df = pd.DataFrame(sorted_ass)

    print(df)


if __name__ == '__main__':
    main()
