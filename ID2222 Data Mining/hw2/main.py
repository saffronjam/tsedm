from singletons import highest_item, find_singletons
from ktons import find_frequent_itemsets


def load_document(filepath: str):
    with open(filepath, 'r') as file:
        lines = [line.rstrip() for line in file]
        return [set([eval(item) for item in line.split(' ')]) for line in lines]


def check(kton, baskets):
    n = 0

    for basket in baskets:

        intersection = basket.intersection(kton)

        if len(intersection) == len(kton):
            n += 1
    print(f'itemset: {kton} had {n} occurances')


def main():
    support = 0.01
    # confidence = XXX
    print("==== FREQUENT ITEMSETS ====")

    print("===== Step 0: Load dataset =====")
    baskets = load_document('dataset')

    print(f'\tsupport: {support}% ({round(support * len(baskets))} items)')

    for kton in [{546, 947, 661, 217, 923}]:
        check(kton, baskets)

    print("===== Step 1: Define range =====")
    max_value = highest_item(baskets)

    print("===== Step 2: Find singletons =====")
    singletons = find_singletons(baskets, max_value, support)
    print(singletons)

    print("===== Step 3: Find k-tons =====")
    itemsets = find_frequent_itemsets(baskets, singletons, support)

    for k, ktons in itemsets.items():
        print(f'{k}: ({len(ktons)}) {ktons}')


if __name__ == '__main__':
    main()
