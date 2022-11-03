from create_shingles import create_shingles

def load_documents(filepath):
    return ['data', 'more data', 'event more data']


def main():

    # Step 1: shingle
    shingle_set = create_shingles(documents=load_documents('some_path.txt'))

    # Step 2:
    # Step 3:
    # Step 4:


if __name__ == '__main__':
    main()