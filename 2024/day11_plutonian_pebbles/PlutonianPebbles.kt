package day11_plutonian_pebbles

import java.io.File

class PlutonianPebbles {

    fun readInput(): String = File("src/day11_plutonian_pebbles/input.txt").readText()

    fun parseInput(line: String): List<ULong> = line.trim().split(" ").map { it.toULong() }

    private fun blink(stone: ULong): List<ULong> = when {
        stone == 0UL -> listOf(1UL)
        stone.toString().length % 2 == 0 -> stone.toString().chunked(stone.toString().length / 2).map { it.toULong() }
        else -> listOf(stone * 2024UL)
    }

    fun numStones(stones: List<ULong>, blinks: Int): ULong {
        val memo = mutableMapOf<Pair<ULong, Int>, ULong>()

        fun calcNumStones(stoneValue: ULong, blinks: Int): ULong = memo.getOrPut(stoneValue to blinks) {
            if (blinks == 0) 1UL else blink(stoneValue).sumOf { calcNumStones(it, blinks - 1) }
        }

        return stones.sumOf { calcNumStones(it, blinks) }
    }

}

fun main() {
    PlutonianPebbles().run {
        val stones = parseInput(readInput())
        println(numStones(stones, 25))
        println(numStones(stones, 75))
    }
}
