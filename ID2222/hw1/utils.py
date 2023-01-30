import numpy as np
from matplotlib import pyplot as plt

def print_heatmap(table, title):
    np_array = np.array(table)

    import seaborn as sns

    fig, (ax1) = plt.subplots(ncols=1, figsize=(12, 6))

    # Change palette:
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
    ax1.set_title(title)
    plt.show()


def print_table(table):
    print('\n'.join(['\t'.join(['{:.2f}'.format(cell) for cell in row]) for row in table]))