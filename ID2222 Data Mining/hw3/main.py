# Homework 3, ID2222
# Emil Karlsson, Pierre Le Fevre

# Count number of triangles using algorithm below:
# https://arxiv.org/pdf/1212.2264.pdf

import random
import time


def count_wedges(edge_t, edge_res):
    # print("     ==> Counting wedges")
    out_degree = {}
    in_degree = {}
    nodes = set()

    edge_t_from = edge_t[0]
    edge_t_to = edge_t[1]

    new_wedges = set()

    for edge in edge_res:
        if edge:
            from_node, to_node = edge

            # Count unique nodes
            nodes.add(from_node)
            nodes.add(to_node)

            # Save wedges that contain edge_t
            if from_node == edge_t_to:
                # edge_t -> shared -> other
                new_wedges.add((edge_t_from, from_node, to_node))
            if to_node == edge_t_from:
                # other -> shared -> edge_t
                new_wedges.add((from_node, to_node, edge_t_to))

            # Count wedges
            out_degree[from_node] = out_degree.get(from_node, 0) + 1
            in_degree[to_node] = in_degree.get(to_node, 0) + 1

    tot_wedges = 0
    for node in nodes:
        # If the node contains both in and out, it must have at least one wedge
        if node in in_degree and node in out_degree:
            tot_wedges += in_degree[node] * out_degree[node]

    return new_wedges, tot_wedges


def main():
    print('=== Mining data streams ===')

    file = open('datasets/p2p-Gnutella08.txt', 'r')
    lines = file.readlines()
    random.shuffle(lines)
    shuffled_lines = lines

    print(f'Found {len(shuffled_lines)} lines')

    # Uniform reservoir of edges
    s_edges = 2000
    edge_res = [None] * s_edges

    # Wedges seen in edge reservoir
    s_wedges = 2000
    wedge_res = [None] * s_wedges

    # is_closed[i] undicates whether wedge_res[i] is closed
    is_closed = [False] * s_wedges

    # Total wedges from edges currently in edge_res
    tot_wedges = 0

    # Timing
    time_init = 0
    time_triangles = 0
    time_new_edge = 0
    time_count_wedges = 0
    time_write_wedges = 0

    t = 0
    t_updated = 0
    updated_edges = 0
    updated_wedges = 0
    while True:
        time_start = time.time()

        # Get next line
        if t == len(shuffled_lines):
            break
        line = shuffled_lines[t]

        # line = file.readline()
        # if not line:
        #     break

        # Skip commented rows
        if line[0] == '#':
            t += 1
            continue

        # Sanitize
        edge_raw = line.replace('\n', '').split('\t')
        edge_t = (int(edge_raw[0]), int(edge_raw[1]))

        # Skip self loops
        if len(set(edge_t)) < 2:
            t += 1
            continue

        t += 1

        time_init += time.time()-time_start
        time_start = time.time()

        # print(" ==> Find which wedges are closed")
        # Find which wedges are closed
        for i, wedge in enumerate(wedge_res):
            if wedge:
                wedge_from, _, wedge_to = wedge
                edge_t_from, edge_t_to = edge_t

                if edge_t_from == wedge_to and edge_t_to == wedge_from:
                    is_closed[i] = True

        time_triangles += time.time()-time_start
        time_start = time.time()

        # print(" ==> Update elements in edge reservoir using probability")
        # Update elements in edge reservoir using probability

        edges_updated = False
        for i in range(s_edges):
            # If random number is between 0 and k, replace the node at that place
            if random.uniform(0, 1) <= 1 / t:
                updated_edges += 1
                edge_res[i] = edge_t
                edges_updated = True

        time_new_edge += time.time()-time_start
        time_start = time.time()

        # print(" ==> Determine which new wedges are created")
        # Determine which new wedges are created
        if edges_updated:
            t_updated += 1

            new_wedges, tot_wedges = count_wedges(edge_t, edge_res)

            time_count_wedges += time.time()-time_start
            time_start = time.time()


            if tot_wedges > 0:
                for i in range(s_wedges):
                    rnd = random.uniform(0, 1)
                    if rnd <= len(new_wedges) / tot_wedges:
                        updated_wedges += 1
                        wedge_res[i] = random.choice(list(new_wedges))
                        is_closed[i] = False

            time_write_wedges += time.time() - time_start

        unique_edges = len(set(edge_res)) / s_edges
        unique_wedges = len(set(wedge_res)) / s_wedges
        non_none = len(list(filter(lambda x: x, wedge_res)))

        p = is_closed.count(True)/s_wedges
        k_t = 3*p
        T_t = (p*(t**2))/(s_edges*(s_edges-1)) * tot_wedges

        if t % 1000 == 0:
            # Init {time_init/t:.4f}, Triangles{time_triangles/t:.4f}, Edges{time_new_edge/t:.4f}, Wedges{time_count_wedges/t_updated:.4f}, Write{time_write_wedges/t_updated:.4f}
            print(f'Time {t}; Triangles {round(T_t)}; Transitivity {k_t:.4f} UpdatedE {updated_edges}; UpdatedW {updated_wedges}; UniqueE {unique_edges}; UniqueW {unique_wedges}; Non-None {non_none}; ', end='\r')
            updated_edges = 0
            updated_wedges = 0
    print(f'Time {t}; Triangles {round(T_t)}; Transitivity {k_t:.4f}')


if __name__ == '__main__':
    main()
