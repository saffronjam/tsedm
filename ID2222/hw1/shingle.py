import binascii

from pyspark.sql.functions import udf
from pyspark.sql.types import ArrayType, BooleanType, IntegerType
from pyspark.sql import SparkSession


def create_shingles(documents, k):
    make_shingles_udf = udf(lambda col: __make_shingles(col, k), ArrayType(IntegerType()))
    shingled_documents = documents.withColumn('shingles', make_shingles_udf(documents.value))

    shingle_column_raw = shingled_documents.select('shingles').collect()
    shingle_column = []
    for row in shingle_column_raw:
        shingle_column.append(set(row.shingles))

    return __create_characteristic(shingle_column), shingled_documents


def __create_characteristic(shingle_column):
    spark = SparkSession.builder \
        .appName('hw1') \
        .getOrCreate()

    all_shingles = set()
    for shingle_set in shingle_column:
        all_shingles = all_shingles.union(shingle_set)

    characteristic = spark.createDataFrame([(shingle,) for shingle in all_shingles], ['shingles'])

    for i, shingle_set in enumerate(shingle_column):
        check_shingle_udf = udf(lambda shingle: True if shingle in shingle_set else False, BooleanType())
        characteristic = characteristic.withColumn(str(i), check_shingle_udf(characteristic.shingles))

    return characteristic


def __make_shingles(document, k):
    shingles = set()
    for i in range(len(document) - k):
        # Hash to 32-bit integer
        hashed_shingle = int(binascii.crc32(document[i:i + k].lower().encode('utf8')) & 0xffffffff)
        shingles.add(hashed_shingle)

    return list(shingles)
