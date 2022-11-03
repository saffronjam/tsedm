import os
import pandas as pd
from shingle import create_shingles


def load_file(filename):
    with open(filename, 'r') as f:
        return f.read()


def load_documents(folder):
    filenames = [f for f in os.listdir(folder) if os.path.isfile(os.path.join(folder, f))]

    filedata = [load_file(folder + '/' + filename) for filename in filenames]

    rows = []
    for i, data in enumerate(filedata):
        rows.append({'i': i, 'raw': data})

    df = pd.DataFrame(rows)

    return df


def main():
    # Step 0: Load dataset
    documents = load_documents('dataset')
    # print(tabulate(documents, headers='keys', tablefmt='psql'))

    # Step 1: shingle
    df = create_shingles(documents)

    print(df)

    # Step 2:
    # Step 3:
    # Step 4:


if __name__ == '__main__':
    main()
