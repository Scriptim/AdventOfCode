#!/usr/bin/env python3

def hash(str: str) -> int:
    value = 0
    for c in str:
        value += ord(c)
        value *= 17
        value %= 256
    return value

def perform_operation(operation: str, boxes: dict[int, dict[str, int]]) -> None:
    if '=' in operation:
        [label, focal_length] = operation.split('=')
    else:
        label, focal_length = operation.split('-')[0], None

    box = hash(label)
    if box not in boxes:
        boxes[box] = {}

    if focal_length is not None:
        boxes[box][label] = int(focal_length)
    elif label in boxes[box]:
        del boxes[box][label]

def focusing_power(boxes: dict[int, dict[str, int]]) -> int:
    power = 0
    for box_num, box in boxes.items():
        for slot_num, focal_length in enumerate(box.values()):
            power += (box_num + 1) * (slot_num + 1) * focal_length
    return power

if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        # parse input
        steps = f.read().strip().split(',')

        # part 1
        result = sum(map(hash, steps))
        assert result == 1320
        print(result)

        # part 2
        boxes = {}
        for step in steps:
            perform_operation(step, boxes)
        result = focusing_power(boxes)
        assert result == 145
        print(result)
