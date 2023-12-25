#!/usr/bin/env python3

def hash(str: str) -> int:
    value = 0
    for c in str:
        value += ord(c)
        value *= 17
        value %= 256
    return value

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        steps = f.read().strip().split(',')

        # part 1
        result = sum(map(hash, steps))
        print(result)
