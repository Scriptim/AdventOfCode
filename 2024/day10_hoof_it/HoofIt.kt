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

    private fun dynamicProgram(
        map: Map<Coordinate, Int>,
        initPeak: (peak: Coordinate) -> Unit,
        update: (current: Coordinate, previous: Coordinate) -> Unit,
    ) {
        val queue = ArrayDeque(map.filterValues { it == 9 }.keys)

        while (queue.isNotEmpty()) {
            val center = queue.removeFirst()

            if (map[center] == 9) initPeak(center)

            Direction.VON_NEUMANN_NEIGHBORHOOD
                .map { center + it }
                .filter { it in map && map[it] == map[center]!! - 1 }
                .forEach {
                    update(it, center)
                    if (it !in queue) queue.addLast(it)
                }
        }
    }

    fun totalTrailheadScore(map: Map<Coordinate, Int>): Int {
        val peaks = map.mapValues { mutableSetOf<Coordinate>() }.toMutableMap()
        dynamicProgram(
            map,
            { peak -> peaks[peak] = mutableSetOf(peak) },
            { current, previous -> peaks[current]!!.addAll(peaks[previous]!!) },
        )
        return map.filterValues { it == 0 }.keys.sumOf { peaks[it]!!.size }
    }

    fun totalTrailheadRating(map: Map<Coordinate, Int>): Int {
        val ratings = map.mapValues { 0 }.toMutableMap()
        dynamicProgram(
            map,
            { peak -> ratings[peak] = 1 },
            { current, previous -> ratings[current] = ratings[current]!! + ratings[previous]!! },
        )
        return map.filterValues { it == 0 }.keys.sumOf { ratings[it]!! }
    }

}

fun main() {
    HoofIt().run {
        val map = parseInput(readInput())
        println(totalTrailheadScore(map))
        println(totalTrailheadRating(map))
    }
}
