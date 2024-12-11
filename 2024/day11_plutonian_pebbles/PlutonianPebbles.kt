package day11_plutonian_pebbles

import java.io.File

class PlutonianPebbles {

    fun readInput(): String = File("src/day11_plutonian_pebbles/input.txt").readText()

    fun parseInput(line: String): List<ULong> = line.trim().split(" ").map { it.toULong() }

    private fun blink(stones: List<ULong>): List<ULong> = stones.flatMap {
        when {
            it == 0UL -> listOf(1UL)
            it.toString().length % 2 == 0 -> it.toString().chunked(it.toString().length / 2).map(String::toULong)
            else -> listOf(it * 2024UL)
        }
    }

    fun numStones(stones: List<ULong>, blinks: Int): Int =
        generateSequence(stones) { blink(it) }.take(blinks + 1).last().size

}

fun main() {
    PlutonianPebbles().run {
        val stones = parseInput(readInput())
        println(numStones(stones, 25))
    }
}
