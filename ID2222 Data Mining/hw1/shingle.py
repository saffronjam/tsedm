def create_shingles(documents):
    # return documents.withColumn('shingles', make_shingles(documents.iloc("raw"), 5))
    documents["shingles"] = documents.apply(lambda i: __make_shingles(i, 5), axis=1, raw=False)  # axis=1
    return documents


def __make_shingles(document, k):
    shingles = []
    for i in range(len(document.raw) - k):
        shingles.append(document.raw[i:i + k].lower())

    return shingles
