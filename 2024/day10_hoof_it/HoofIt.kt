package day10_hoof_it

import Direction
import Vector2D
import parse2DMap
import plus
import toDigit
import java.io.File

class HoofIt {

    fun readInput(): List<String> = File("src/day10_hoof_it/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Vector2D<Int>, Int> = parse2DMap(lines).mapValues { it.value.toDigit() }

    private fun dynamicProgram(
        map: Map<Vector2D<Int>, Int>,
        initPeak: (peak: Vector2D<Int>) -> Unit,
        update: (current: Vector2D<Int>, previous: Vector2D<Int>) -> Unit,
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

    fun totalTrailheadScore(map: Map<Vector2D<Int>, Int>): Int {
        val peaks = map.mapValues { mutableSetOf<Vector2D<Int>>() }.toMutableMap()
        dynamicProgram(
            map,
            { peak -> peaks[peak] = mutableSetOf(peak) },
            { current, previous -> peaks[current]!!.addAll(peaks[previous]!!) },
        )
        return map.filterValues { it == 0 }.keys.sumOf { peaks[it]!!.size }
    }

    fun totalTrailheadRating(map: Map<Vector2D<Int>, Int>): Int {
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
