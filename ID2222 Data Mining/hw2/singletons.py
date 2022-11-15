def highest_item(baskets):
    largest = 0
    for basket in baskets:
        for item in basket:
            largest = max(item, largest)
    return largest


def find_singletons(baskets, max_value, support):
    counts = [0 for _ in range(max_value + 1)]

    for basket in baskets:
        for item in basket:
            counts[item] += 1

    singletons = []
    for i, item_count in enumerate(counts):
        if item_count / len(baskets) > support:
            singletons.append(i)

    return singletons
