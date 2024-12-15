package day15_warehouse_woes

import Direction
import Vector2D
import parse2DMap
import plus
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
                else -> throw IllegalArgumentException("Invalid movement: $it")
            }
        }

        return Pair(map, movements)
    }

    private fun move(
        map: MutableMap<Vector2D<Int>, Char>, position: Vector2D<Int>, direction: Direction
    ): Vector2D<Int> {
        var destination = position
        do {
            destination += direction
        } while (map[destination] == 'O')

        if (map[destination] == '#') return position

        map[destination] = map[position + direction]!!
        map[position + direction] = map[position]!!
        map[position] = '.'
        return position + direction
    }

    fun gpsSum(map: Map<Vector2D<Int>, Char>, movements: List<Direction>): Int {
        val mutMap = map.toMutableMap()
        var position = mutMap.filterValues { it == '@' }.keys.first()
        movements.forEach { movement -> position = move(mutMap, position, movement) }
        return mutMap.filterValues { it == 'O' }.keys.sumOf { it.x + 100 * it.y }
    }

}

fun main() {
    WarehouseWoes().run {
        val (map, movements) = parseInput(readInput())
        println(gpsSum(map, movements))
    }
}
