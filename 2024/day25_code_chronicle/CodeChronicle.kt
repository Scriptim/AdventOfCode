package day25_code_chronicle

import java.io.File

class CodeChronicle {

    private val schematicHeight = 7

    fun readInput(): List<String> = File("src/day25_code_chronicle/input.txt").readLines()

    fun parseInput(lines: List<String>): Pair<Set<List<Int>>, Set<List<Int>>> {
        val locks = mutableSetOf<List<Int>>()
        val keys = mutableSetOf<List<Int>>()

        lines.chunked(schematicHeight + 1).map { it.takeWhile(String::isNotBlank) }.forEach { schematic ->
            val heights = schematic.first().indices.map { column -> schematic.count { it[column] == '#' } - 1 }
            if (schematic.first().contains('#')) locks.add(heights) else keys.add(heights)
        }

        return Pair(locks.toSet(), keys.toSet())
    }

    private fun fit(lock: List<Int>, key: List<Int>): Boolean =
        lock.zip(key, Int::plus).all { it <= schematicHeight - 2 }

    fun fittingKeyLockPairs(locks: Set<List<Int>>, keys: Set<List<Int>>): Int =
        locks.sumOf { lock -> keys.count { key -> fit(lock, key) } }

}

fun main() {
    CodeChronicle().run {
        val (locks, keys) = parseInput(readInput())
        println(fittingKeyLockPairs(locks, keys))
    }
}
