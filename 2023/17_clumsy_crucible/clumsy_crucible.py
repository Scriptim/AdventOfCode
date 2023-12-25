#!/usr/bin/env python3

import dijkstar
from math import copysign

def build_graph(heat_loss_map: [[int]], min_move: int = 1, max_move: int = 3) -> dijkstar.Graph:
    graph = dijkstar.Graph()
    height = len(heat_loss_map)
    width = len(heat_loss_map[0])

    for v1, v2 in [
        ('start', ((0, 0), True)),
        ('start', ((0, 0), False)),
        (((width - 1, height - 1), True), 'destination'),
        (((width - 1, height - 1), False), 'destination'),
    ]:
        graph.add_edge(v1, v2, 0)

    for y in range(height):
        for x in range(width):
            for vertical in [True, False]:
                for delta in list(range(min_move, max_move + 1)) + list(range(-min_move, -max_move - 1, -1)):
                    neighbor_x, neighbor_y = (x + delta * (not vertical), y + delta * vertical)
                    if not (0 <= neighbor_x < width and 0 <= neighbor_y < height):
                        continue

                    sign = int(copysign(1, delta))
                    if abs(delta) == min_move:
                        weight = sum(heat_loss_map[y + i * vertical][x + i * (not vertical)] for i in range(sign, min_move * sign, sign))
                    weight += heat_loss_map[neighbor_y][neighbor_x]
                    graph.add_edge(((x, y), vertical), ((neighbor_x, neighbor_y), not vertical), weight)

    return graph

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        heat_loss_map = [[int(c) for c in line.strip()] for line in f.readlines()]

        # part 1
        graph = build_graph(heat_loss_map)
        result = dijkstar.find_path(graph, 'start', 'destination').total_cost
        print(result)

        # part 2
        graph = build_graph(heat_loss_map, 4, 10)
        result = dijkstar.find_path(graph, 'start', 'destination').total_cost
        print(result)
