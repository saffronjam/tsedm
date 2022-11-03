def create_shingles(documents):
    # return documents.withColumn('shingles', make_shingles(documents.iloc("raw"), 5))
    documents["shingles"] = documents.apply(lambda i, raw: make_shingles(raw, 5), axis=1, raw=False)  # axis=1
    return documents


def make_shingles(document, k):
    shingles = set()
    for i in range(len(document) - k):
        shingles.add(document[i:i + k])

    return "test"
