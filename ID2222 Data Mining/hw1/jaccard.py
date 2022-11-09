from pyspark.sql.functions import when
from utils import print_heatmap, print_table

def compare_cols(col1, col2):
    type_a = 0
    type_b = 0
    type_c = 0

    for i in range(len(col1)):
        if col1[i] and col2[i]:
            type_a += 1
        elif col1[i] and not col2[i]:
            type_b += 1
        elif not col1[i] and col2[i]:
            type_c += 1

    return type_a / (type_a + type_b + type_c)


def make_matrix(df):
    result = []

    for i in range(len(df.columns) - 1):
        result.append([])

        for j in range(len(df.columns) - 1):
            cleaned = df.where(df[str(i)] | df[str(j)])

            type_a = cleaned.where(df[str(i)] & df[str(j)]).count()
            type_b = cleaned.where(df[str(i)] & ~ df[str(j)]).count()
            type_c = cleaned.where(~ df[str(i)] & df[str(j)]).count()
            #
            # jaccard = df.select(when(df[str(i)] & df[str(j)], 0)
            #                     .when(df[str(i)] & ~ df[str(j)], 1)
            #                     .when(~ df[str(i)] & df[str(j)], 2)
            #                     .alias('result')).collect()

            # type_a, type_b, type_c = 0, 0, 0

            # print(jaccard)
            # print("")
            # for jac in jaccard:
            #     if jac == 0:
            #         type_a += 1
            #     elif jac == 2:
            #         type_b += 1
            #     elif jac == 2:
            #         type_c += 1

            comparison_result = type_a / (type_a + type_b + type_c)
            result[i].append(comparison_result)

            print(f'Result for {i} and {j}: {comparison_result}')

    print_table(result)
    print_heatmap(result, "Jaccard")
