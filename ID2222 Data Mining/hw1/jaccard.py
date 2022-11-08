import numpy as np
from matplotlib import pyplot as plt


def print_heatmap(table):
    np_array = np.array(table)

    import seaborn as sns

    fig, (ax1) = plt.subplots(ncols=1, figsize=(12, 6))

    # Chane palette:
    # https://seaborn.pydata.org/tutorial/color_palettes.html
    color_palette = sns.color_palette("viridis", as_cmap=True)
    sns.heatmap(data=np_array,
                ax=ax1,
                cmap=color_palette,
                annot=True,
                annot_kws={'fontsize': 16, 'fontweight': 'bold'},
                cbar_kws={'orientation': 'vertical'})

    colorbar = ax1.collections[0].colorbar
    colorbar.ax.tick_params(labelsize=20, colors='black')

    plt.show()


def print_table(table):
    print('\n'.join(['\t'.join(['{:.2f}'.format(cell) for cell in row]) for row in table]))


def compare_sets(set1, set2):
    intersect = set1.intersection(set2)
    union = set1.union(set2)

    return len(intersect) / len(union)

    # The Jaccard distance is 1 minus the ratio of the sizes of the
    # intersection and union of sets x and y.


def make_matrix(df):
    collected = df.collect()

    result = []

    for i in range(df.count()):
        result.append([])
        for j in range(df.count()):
            set1 = set(collected[i].shingles)
            set2 = set(collected[j].shingles)
            result[i].append(compare_sets(set1, set2))

    print_table(result)
    print_heatmap(result)
