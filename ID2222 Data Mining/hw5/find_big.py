import os
import sys


def get_last_lines():
    last_lines = {}
    folder = 'output'
    if (len(sys.argv) >= 2):
        folder = 'output-' + sys.argv[1]

    for filename in os.listdir(folder):
        with open(folder + '/' + filename, "rb") as file:
            try:
                file.seek(-2, os.SEEK_END)
                while file.read(1) != b'\n':
                    file.seek(-2, os.SEEK_CUR)
            except OSError:
                file.seek(0)
            last_line = file.readline().decode()
            last_lines[filename] = last_line
    return last_lines


def find_largest(lines):
    # Round		Edge-Cut		Swaps		Migrations		Skipped

    best_line = None
    best_edge_cut = 2**32
    best_filename = None

    for filename, line in lines.items():
        edge_cut = int(line.split('\t\t')[1])
        if edge_cut < best_edge_cut:
            best_line = line
            best_edge_cut = edge_cut
            best_filename = filename

    return best_filename, best_line, best_edge_cut


def main():
    last_lines = get_last_lines()
    filename, line, edge_cut = find_largest(last_lines)

    print(f'best edge cut: {edge_cut} ({filename})')

    folder = 'output'
    if (len(sys.argv) >= 2):
        folder = 'output-' + sys.argv[1]

    os.system('./plot.sh ' + folder + "/" + filename)


if __name__ == '__main__':
    main()
