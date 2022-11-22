# Homework 3, ID2222
# Emil Karlsson, Pierre Le Fevre

# Count number of triangles using algorithm below:
# https://arxiv.org/pdf/1212.2264.pdf

import random



def calculate_wedges(edge_res):


def main():
    print('=== Mining data streams ===')

    file = open('datasets/web-Stanford.txt', 'r')

    # Uniform reservoir of edges
    edge_res = []

    # Wedges seen in edge reservoir
    wedge_res = []

    # is_closed[i] undicates whether wedge_res[i] is closed
    is_closed = []

    # Total wedges so far
    tot_wedges = 0

    k = 40000
    i = -1

    while True:
        i += 1

        # Get next line
        line = file.readline()
        if not line:
            break

        # Skip commented rows
        if line[0] == '#':
            continue
        
        # Fill edge reservoir at the start
        nodes = line.split('\t')
        if len(edge_res < k):
            edge_res.append(nodes)
            wedge_res = calculate_wedges(edge_res)
            continue

        # If random number is between 0 and k, replace the node at that place
        rnd = random.randint(0, i)
        if rnd < k:
            edge_res[k] = nodes
            wedge_res = calculate_wedges(edge_res)

        

        



        



if __name__ == '__main__':
    main()
