package day10_hoof_it

import Coordinate
import Direction
import plus
import java.io.File

class HoofIt {

    fun readInput(): List<String> = File("src/day10_hoof_it/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Coordinate, Int> = lines.flatMapIndexed { row, line ->
        line.mapIndexed { column, char -> Pair(column, row) to char.toString().toInt() }
    }.toMap()

    private fun reachablePeaks(map: Map<Coordinate, Int>): Map<Coordinate, Set<Coordinate>> {
        val peaks = map.mapValues { mutableSetOf<Coordinate>() }.toMutableMap()
        val queue = ArrayDeque(map.filterValues { it == 9 }.keys)

        while (queue.isNotEmpty()) {
            val center = queue.removeFirst()

            if (map[center] == 9) peaks[center] = mutableSetOf(center)

            Direction.VON_NEUMANN_NEIGHBORHOOD
                .map { center + it }
                .filter { it in map && map[it] == map[center]!! - 1 }
                .forEach {
                    peaks[it]!!.addAll(peaks[center]!!)
                    if (it !in queue) queue.add(it)
                }
        }

        return peaks.mapValues { it.value.toSet() }.toMap()
    }

    fun totalTrailheadScore(map: Map<Coordinate, Int>): Int =
        reachablePeaks(map).let { peaks -> map.filterValues { it == 0 }.keys.sumOf { peaks[it]!!.size } }

}

fun main() {
    HoofIt().run {
        val map = parseInput(readInput())
        println(totalTrailheadScore(map))
    }
}
