from typing import List, Set


def find_singletons(baskets, max_value, support):
    counts = [0 for _ in range(max_value + 1)]

    singletons = []
    singletons_per_basket = dict()

    for i, basket in enumerate(baskets):
        for item in basket:
            counts[item] += 1
            basket_list = singletons_per_basket.get(item , [])
            basket_list.append(i)
            singletons_per_basket[item] = basket_list

    for item, item_count in enumerate(counts):
        if item_count / len(baskets) > support:
            singletons.append(item)
        else:
            if item in singletons_per_basket:
                del singletons_per_basket[item]

    return singletons, singletons_per_basket


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

    frequent = set()
    frequent_per_basket = dict()

    for i, basket in enumerate(baskets):
        singletons_in_basket = sorted([item for item in basket if item in singletons])
        prev_ktons_in_basket = sorted([kton for kton in prev_ktons if kton_in_basket(kton, basket)])

        for newple in gen_ktons(singletons_in_basket, prev_ktons_in_basket):
            itemsets[newple] = itemsets.get(newple, 0) + 1
            basket_list = frequent_per_basket.get(newple, [])
            basket_list.append(i)
            frequent_per_basket[newple] = basket_list

    # Filter out any itemsets that do not occur often enough
    for newple, occurrences in itemsets.items():
        if occurrences / len(baskets) > support:
            frequent.add(newple)
        else:
            if newple in frequent_per_basket:
                del frequent_per_basket[newple]

    return frequent, frequent_per_basket


def find_frequent_itemsets(baskets: List[Set[int]], max_value, support):
    singletons, singletons_per_basket = find_singletons(baskets, max_value, support)

    itemsets = {1: [{item} for item in singletons]}
    itemsets_per_basket = singletons_per_basket

    i = 2
    while True:
        ktons, ktons_per_basket = get_frequent_ktons(baskets, singletons, itemsets[i - 1], support)
        ktons = [set(item) for item in ktons]
        if len(ktons) == 0:
            break
        itemsets[i] = ktons
        itemsets_per_basket = itemsets_per_basket | ktons_per_basket
        i += 1

    return itemsets, itemsets_per_basket
