package day20_race_condition

import Direction
import Vector2D
import manhattan
import minus
import parse2DMap
import plus
import java.io.File
import java.util.*
import kotlin.math.abs

class RaceCondition {

    fun readInput(): List<String> = File("src/day20_race_condition/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Vector2D<Int>, Char> = parse2DMap(lines)

    private fun shortestPaths(map: Map<Vector2D<Int>, Char>): Map<Vector2D<Int>, Int> {
        val end = map.entries.first { it.value == 'E' }.key
        val distance = mutableMapOf(end to 0).withDefault { Int.MAX_VALUE }
        val queue = PriorityQueue<Vector2D<Int>>(compareBy { distance.getValue(it) }).apply { add(end) }

        while (queue.isNotEmpty()) {
            val current = queue.remove()
            Direction.VON_NEUMANN_NEIGHBORHOOD
                .map { current + it }
                .filter { it in map && map.getValue(it) != '#' }
                .filter { distance[current]!! + 1 < distance.getValue(it) }
                .forEach {
                    distance[it] = distance[current]!! + 1
                    queue.add(it)
                }
        }

        return distance.toMap()
    }

    private fun neighbors(center: Vector2D<Int>, radius: Int): Set<Vector2D<Int>> =
        (-radius..radius).flatMap { dx ->
            (-(radius - abs(dx))..(radius - abs(dx))).map { dy ->
                Vector2D(dx, dy)
            }
        }.filter { abs(it.x) + abs(it.y) <= radius }.map { center + it }.toSet()

    fun countGoodCheats(map: Map<Vector2D<Int>, Char>, timeLimit: Int): Int {
        val distance = shortestPaths(map)
        val raceTrack = map.filterValues { it != '#' }.keys
        return raceTrack.sumOf { trackTile ->
            neighbors(trackTile, timeLimit).count {
                it in raceTrack && distance[trackTile]!! - (distance[it]!! + (trackTile - it).manhattan()) >= 100
            }
        }
    }

}

fun main() {
    RaceCondition().run {
        val map = parseInput(readInput())
        println(countGoodCheats(map, timeLimit = 2))
        println(countGoodCheats(map, timeLimit = 20))
    }
}
