#!/usr/bin/env python3

import igraph
from math import copysign

def build_graph(heat_loss_map: [[int]], min_move: int = 1, max_move: int = 3) -> igraph.Graph:
    height = len(heat_loss_map)
    width = len(heat_loss_map[0])

    vertices = ['start', 'destination']
    edges = [
        ('start', ((0, 0), True)),
        ('start', ((0, 0), False)),
        (((width - 1, height - 1), True), 'destination'),
        (((width - 1, height - 1), False), 'destination'),
    ]
    weights = [0, 0, 0, 0]

    for y in range(height):
        for x in range(width):
            for vertical in [True, False]:
                vertex = ((x, y), vertical)
                vertices.append(vertex)

                for delta in (d for d in range(-max_move, max_move + 1) if abs(d) >= min_move):
                    neighbor_x, neighbor_y = (x + delta * (not vertical), y + delta * vertical)
                    if not (0 <= neighbor_x < width and 0 <= neighbor_y < height):
                        continue

                    edges.append((vertex, ((neighbor_x, neighbor_y), not vertical)))
                    sign = int(copysign(1, delta))
                    weight = sum(heat_loss_map[y + i * vertical][x + i * (not vertical)]
                                 for i in range(sign, delta + sign, sign))
                    weights.append(weight)

    edges = map(lambda e: (vertices.index(e[0]), vertices.index(e[1])), edges)
    graph = igraph.Graph(edges, directed=True)
    graph.es['weight'] = weights
    return graph

def shortest_path_weight(heat_loss_map: [[int]], min_move: int = 1, max_move: int = 3) -> int:
    graph = build_graph(heat_loss_map, min_move, max_move)
    shortest_path = graph.get_shortest_path(0, 1, weights='weight', output='epath')
    return sum(map(lambda e: graph.es[e]['weight'], shortest_path))

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        heat_loss_map = [[int(c) for c in line.strip()] for line in f.readlines()]

        # part 1
        result = shortest_path_weight(heat_loss_map)
        print(result)

        # part 2
        result = shortest_path_weight(heat_loss_map, 4, 10)
        print(result)
