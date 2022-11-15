from typing import List, Set


def kton_in_basket(kton, basket):
    for item in kton:
        if item not in basket:
            return False
    return True


def gen_ktons(singletons, prev_ktons):
    possible_kton_perms = list()

    for kton in prev_ktons:
        for singleton in singletons:
            # doubleton + singleton = tripleton etc.
            if singleton not in kton:
                newple = kton.union({singleton})
                possible_kton_perms.append(tuple(sorted(list(newple))))

    return list(set(possible_kton_perms))


def get_frequent_ktons(baskets, singletons, prev_ktons, support):
    itemsets = {}

    for basket in baskets:
        singletons_in_basket = sorted([item for item in basket if item in singletons])
        prev_ktons_in_basket = sorted([kton for kton in prev_ktons if kton_in_basket(kton, basket)])

        for newple in gen_ktons(singletons_in_basket, prev_ktons_in_basket):
            itemsets[newple] = itemsets.get(newple, 0) + 1

    # Filter out any itemsets that do not occur often enough
    frequent = set()
    for newple, occurrences in itemsets.items():
        if occurrences / len(baskets) > support:
            frequent.add(newple)

    return frequent


def find_frequent_itemsets(baskets: List[Set[int]], singletons, support):
    itemsets = {1: [{item} for item in singletons]}

    i = 2
    while True:
        ktons = get_frequent_ktons(baskets, singletons, itemsets[i - 1], support)
        ktons = [set(item) for item in ktons]
        if len(ktons) == 0:
            break
        itemsets[i] = ktons
        i += 1

    return itemsets
