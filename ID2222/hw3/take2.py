import random
import time


def count_wedges(edge_res):
    tot_wedges = 0
    degrees = {}
    for edge in edge_res:
        degrees[edge[0]] = degrees.get(edge[0], 0) + 1
        degrees[edge[1]] = degrees.get(edge[1], 0) + 1

    for degree in degrees.values():
        tot_wedges += degree * (degree-1) / 2

    return tot_wedges


def get_new_wedges(edge_res, edge_t):
    new_wedges = []
    for edge in edge_res:
        if edge:
            nodes = [edge_t[0], edge_t[1], edge[0], edge[1]]
            nodes_set = set(nodes)
            for node in nodes_set:
                nodes.remove(node)

            if len(nodes) == 1:
                nodes_set.remove(nodes[0])
                new_wedges.append(list(nodes_set))

    return new_wedges


def plot(data, name):
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(nrows=1, ncols=1)
    plt.title(name)
    ax.plot(range(len(data)), data)
    fig.savefig(name + '.png')


def main():
    # Options
    print_timers = False

    # Dataset
    dataset = 'facebook'
    file = open(f'datasets/{dataset}.txt', 'r')
    lines = file.readlines()
    random.shuffle(lines)
    split = ' '

    # Plot options
    steps = 100
    plot_ratio = int(len(lines) / steps)
    triangles_plot_data = []
    transitivity_plot_data = []

    # Timers
    timer_update_closed = 0
    timer_update_edge_res = 0
    timer_count_wedges = 0
    timer_get_new_wedges = 0
    timer_update_wedge_res = 0

    # two orders of magnitude smaller than graph
    s_edge = int(0.01*len(lines))
    edge_res = [None]*s_edge

    s_wedge = int(0.01*len(lines))
    wedge_res = [None]*s_wedge
    is_closed = [False]*s_wedge

    for i, line in enumerate(lines):
        t = i + 1
        line_split = line.split(split)
        edge_t = (int(line_split[0]), int(line_split[1]))

        # Step 1: Check wedges closed by edge_t
        now = time.time()
        for i in range(s_wedge):
            if wedge_res[i]:
                edges = set(
                    [edge_t[0], edge_t[1], wedge_res[i][0], wedge_res[i][1]])
                if len(edges) == 2:
                    is_closed[i] = True
        timer_update_closed += time.time() - now

        # Step 2: Update edge reservoir with 1/t
        now = time.time()
        updated_edge_res = False
        for i in range(s_edge):
            x = random.uniform(0, 1)
            if x <= 1/t:
                edge_res[i] = edge_t
                updated_edge_res = True
        timer_update_edge_res += time.time() - now

        if updated_edge_res:
            # Step 3: Count wedges formed by new edge reservoir
            now = time.time()
            tot_wedges = count_wedges(edge_res)
            timer_count_wedges += time.time() - now

            # Step 4: Find new wedges including edge_t
            now = time.time()
            n_t = get_new_wedges(edge_res, edge_t)
            new_wedges = len(n_t)
            timer_get_new_wedges += time.time() - now

            if tot_wedges > 0:

                # Step 5: Update wedge reservoir
                now = time.time()
                for i in range(s_wedge):
                    x = random.uniform(0, 1)
                    if x <= new_wedges/tot_wedges:
                        wedge_res[i] = random.choice(n_t)
                        is_closed[i] = False
                timer_update_wedge_res += time.time() - now

        p = is_closed.count(True)/s_wedge
        k_t = 3*p
        T_t = (p*t**2/(s_edge*(s_edge-1)))*tot_wedges

        if t % plot_ratio == 0:
            if print_timers:
                print(f"Time {t}; Upt Closed {timer_update_closed:.3f}; Upt E Res {timer_update_edge_res:.3f}; Cnt Wedges {timer_count_wedges:.3f}; New Wedges {timer_get_new_wedges:.3f} Upt W Res {timer_update_wedge_res:.3f}", end='\r')
            else:
                print(f"Time {t}; Triangles {T_t:.0f}; Transitivity {k_t:.4f}")

            triangles_plot_data.append(T_t)
            transitivity_plot_data.append(k_t)

    if print_timers:
        print(f"Time {t}; Upt Closed {timer_update_closed:.3f}; Upt E Res {timer_update_edge_res:.3f}; Cnt Wedges {timer_count_wedges:.3f}; New Wedges {timer_get_new_wedges:.3f} Upt W Res {timer_update_wedge_res:.3f}")
    else:
        print(f"Time {t}; Triangles {T_t:.0f}; Transitivity {k_t:.4f}")
    plot(triangles_plot_data, f"triangles-{dataset}")
    plot(transitivity_plot_data, f"transitivity-{dataset}")


if __name__ == '__main__':
    main()
