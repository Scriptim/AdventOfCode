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

def energize(grid: [str]) -> int:
    beams = [((0, 0), (1, 0))]
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
        result = energize(grid)
        print(result)
