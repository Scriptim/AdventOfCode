#!/usr/bin/env python3

import igraph

def build_graph(heat_loss_map: [[int]]) -> igraph.Graph:
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

                for delta in [1, 2, 3, -1, -2, -3]:
                    neighbor_x, neighbor_y = (x, y + delta) if vertical else (x + delta, y)
                    if not (0 <= neighbor_x < width and 0 <= neighbor_y < height):
                        continue

                    neighbor = ((neighbor_x, neighbor_y), not vertical)
                    edges.append((vertex, neighbor))
                    weight = heat_loss_map[neighbor_y][neighbor_x] + (weights[-1] if abs(delta) > 1 else 0)
                    weights.append(weight)

    edges = map(lambda e: (vertices.index(e[0]), vertices.index(e[1])), edges)
    graph = igraph.Graph(edges, directed=True)
    graph.es['weight'] = weights
    return graph

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        heat_loss_map = [[int(c) for c in line.strip()] for line in f.readlines()]

        # part 1
        graph = build_graph(heat_loss_map)
        shortest_path = graph.get_shortest_path(0, 1, weights='weight', output='epath')
        result = sum(map(lambda e: graph.es[e]['weight'], shortest_path))
        print(result)
