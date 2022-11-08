from pyspark.sql import SparkSession
from pyspark.sql.functions import udf
from pyspark.sql.types import StructType, FloatType


def print_table(table):
    print('\n'.join(['\t'.join([str(cell) for cell in row]) for row in table]))



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
        result.append([i])
        for j in range(df.count()):
            result[i + 1].append(j)
            set1 = set(collected[i].shingles)
            set2 = set(collected[j].shingles)
            result[i].append("{:.2f}".format(compare_sets(set1, set2)))

    print_table(result)
