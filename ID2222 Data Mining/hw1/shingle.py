from pyspark.sql.functions import udf
from pyspark.sql.types import ArrayType, StringType


def create_shingles(documents, k):
    my_udf = udf(lambda col: __make_shingles(col, k), ArrayType(StringType()))
    return documents.withColumn('shingles', my_udf(documents.value))


def __make_shingles(document, k):
    shingles = []
    for i in range(len(document) - k):
        shingles.append(hash(document[i:i + k].lower()))

    return list(set(shingles))
