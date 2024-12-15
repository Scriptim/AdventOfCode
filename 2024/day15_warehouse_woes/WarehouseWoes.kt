package day15_warehouse_woes

import Direction
import Vector2D
import minus
import parse2DMap
import plus
import replaceStraightLine
import times
import java.io.File

class WarehouseWoes {

    fun readInput(): List<String> = File("src/day15_warehouse_woes/input.txt").readLines()

    fun parseInput(lines: List<String>): Pair<Map<Vector2D<Int>, Char>, List<Direction>> {
        val mapLines = lines.takeWhile { it.isNotEmpty() }
        val map = parse2DMap(mapLines)

        val movementLines = lines.drop(mapLines.size + 1)
        val movements = movementLines.joinToString("").map {
            when (it) {
                '^' -> Direction.NORTH
                '>' -> Direction.EAST
                'v' -> Direction.SOUTH
                '<' -> Direction.WEST
                else -> throw IllegalArgumentException(it.toString())
            }
        }

        return Pair(map, movements)
    }

    private fun makeWider(map: Map<Vector2D<Int>, Char>): Map<Vector2D<Int>, Char> =
        map.entries.flatMap { (position, char) ->
            listOf(
                Vector2D(position.x * 2, position.y) to when (char) {
                    '@' -> '@'
                    'O' -> '['
                    else -> char
                },
                Vector2D(position.x * 2 + 1, position.y) to when (char) {
                    '@' -> '.'
                    'O' -> ']'
                    else -> char
                },
            )
        }.toMap()

    private fun move(
        map: MutableMap<Vector2D<Int>, Char>, position: Vector2D<Int>, direction: Direction
    ): Vector2D<Int> {
        var destination = position
        do {
            destination += direction
        } while (map[destination] == 'O')

        if (map[destination] == '#') return position

        map[destination] = map[position + direction]!!
        map.replaceStraightLine(position, ".@", direction)
        return position + direction
    }

    private fun moveWide(
        map: MutableMap<Vector2D<Int>, Char>, position: Vector2D<Int>, direction: Direction
    ): Vector2D<Int> {
        val boxes = mutableSetOf<Vector2D<Int>>()

        fun collectBoxes(box: Vector2D<Int>): Boolean = when (map[box]) {
            ']' -> collectBoxes(box + Direction.WEST)
            '@' -> collectBoxes(box + direction)
            '.' -> true
            '#' -> false
            else -> {
                boxes.add(box)
                when (direction) {
                    Direction.EAST -> collectBoxes(box + direction * 2)
                    Direction.WEST -> collectBoxes(box + direction)
                    else -> collectBoxes(box + direction) && collectBoxes(box + Direction.EAST + direction)
                }
            }
        }

        if (!collectBoxes(position)) return position

        boxes.sortedBy {
            when (direction) {
                Direction.NORTH -> it.y
                Direction.EAST -> -it.x
                Direction.SOUTH -> -it.y
                Direction.WEST -> it.x
                else -> throw IllegalArgumentException(direction.toString())
            }
        }.forEach {
            when (direction) {
                Direction.EAST -> map.replaceStraightLine(it, ".[]", direction)
                Direction.WEST -> map.replaceStraightLine(it - direction, ".][", direction)
                else -> {
                    map.replaceStraightLine(it + direction, "[]", Direction.EAST)
                    map.replaceStraightLine(it, "..", Direction.EAST)
                }
            }
        }

        map.replaceStraightLine(position, ".@", direction)
        return position + direction
    }

    fun gpsSum(map: Map<Vector2D<Int>, Char>, movements: List<Direction>, wide: Boolean = false): Int {
        val mutMap = (if (wide) makeWider(map) else map).toMutableMap()

        var position = mutMap.filterValues { it == '@' }.keys.first()
        movements.forEach { position = if (wide) moveWide(mutMap, position, it) else move(mutMap, position, it) }

        return mutMap.filterValues { it == 'O' || it == '[' }.keys.sumOf { it.x + 100 * it.y }
    }

}

fun main() {
    WarehouseWoes().run {
        val (map, movements) = parseInput(readInput())
        println(gpsSum(map, movements))
        println(gpsSum(map, movements, wide = true))
    }
}
