#!/usr/bin/env python3

def beam_step(beam: tuple[tuple[int, int], tuple[int, int]], grid: [str]) -> [tuple[tuple[int, int], tuple[int, int]]]:
    (x, y), (dx, dy) = beam
    beams = []
    if grid[y][x] == '/':
        beams = [((x - dy, y - dx), (-dy, -dx))]
    elif grid[y][x] == '\\':
        beams = [((x + dy, y + dx), (dy, dx))]
    elif grid[y][x] == '|' and dx != 0:
        beams = [((x, y - 1), (0, -1)), ((x, y + 1), (0, 1))]
    elif grid[y][x] == '-' and dy != 0:
        beams = [((x - 1, y), (-1, 0)), ((x + 1, y), (1, 0))]
    else:
        beams = [((x + dx, y + dy), (dx, dy))]
    return list(filter(lambda beam: 0 <= beam[0][0] < len(grid[0]) and 0 <= beam[0][1] < len(grid), beams))

def energize(beam: tuple[tuple[int, int], tuple[int, int]], grid: [str]) -> int:
    beams = [beam]
    beam_history = set(beams)
    stable = False
    while not stable:
        stable = True
        new_beams = []
        for beam in beams:
            for new_beam in beam_step(beam, grid):
                if new_beam not in beam_history:
                    stable = False
                    beam_history.add(new_beam)
                    new_beams.append(new_beam)
        beams = new_beams
    return len(set(map(lambda beam: beam[0], beam_history)))

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        grid = [line.strip() for line in f.readlines()]

        # part 1
        result = energize(((0, 0), (1, 0)), grid)
        assert result == 46
        print(result)

        # part 2
        edges = []
        for y in range(len(grid)):
            edges.append(((0, y), (1, 0)))
            edges.append(((len(grid[0]) - 1, y), (-1, 0)))
        for x in range(len(grid[0])):
            edges.append(((x, 0), (0, 1)))
            edges.append(((x, len(grid) - 1), (0, -1)))
        result = max(map(lambda edge: energize(edge, grid), edges))
        assert result == 51
        print(result)
