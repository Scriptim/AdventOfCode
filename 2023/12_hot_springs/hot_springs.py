#!/usr/bin/env python3

from functools import cache

@cache
def num_arrangements(record: str, groups: tuple[int, ...]) -> int:
    record = record.lstrip('.')

    if not groups:
        return 0 if '#' in record else 1
    group = groups[0]
    if len(record) < group:
        return 0
    if record[0] == '?':
        return num_arrangements(record[1:], groups) + num_arrangements('#' + record[1:], groups)
    if all(c in '?#' for c in record[:group]) and (len(record) == group or record[group] != '#'):
        return num_arrangements(record[group + 1:], groups[1:])
    return 0

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        records = []
        for line in f.readlines():
            [record, groups] = line.strip().split()
            records.append((record, tuple(map(int, groups.split(',')))))

        # part 1
        result = 0
        for record, groups in records:
            result += num_arrangements(record, groups)
        assert result == 21
        print(result)

        # part 2
        result = 0
        for record, groups in records:
            record = '?'.join([''.join(record)] * 5)
            groups = groups * 5
            result += num_arrangements(record, groups)
        assert result == 525152
        print(result)
