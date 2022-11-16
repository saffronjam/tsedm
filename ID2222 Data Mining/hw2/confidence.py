import functools


def generate_associations(baskets, itemsets_per_basket, max_value, confidence_threshold):
    all_items = list(range(max_value + 1))

    associations = []

    for itemset, basket_ids in itemsets_per_basket.items():
        for item in all_items:
            # For each itemset, check if it implies item ( J -> i)

            # Find confidence by going through every basket that has the itemset
            item_support = 0

            for basket_id in basket_ids:
                basket = baskets[basket_id]

                if item in basket:
                    if isinstance(itemset, int):
                        if item != itemset:
                            item_support += 1
                    elif item not in set(itemset):
                        item_support += 1

            confidence = item_support / len(basket_ids)
            if confidence > confidence_threshold:
                associations.append({'from': itemset, 'to': item, 'confidence': confidence})

    return associations
