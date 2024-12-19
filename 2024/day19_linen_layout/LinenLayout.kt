package day19_linen_layout

import java.io.File

class LinenLayout {

    fun readInput(): List<String> = File("src/day19_linen_layout/input.txt").readLines()

    fun parseInput(lines: List<String>): Pair<List<String>, List<String>> {
        val towels = lines.first().split(", ")
        val designs = lines.drop(2)
        return Pair(towels, designs)
    }

    private fun designPossible(towels: List<String>, design: String): Boolean =
        design.isEmpty() || towels.any { design.endsWith(it) && designPossible(towels, design.dropLast(it.length)) }

    fun countPossibleDesigns(towels: List<String>, designs: List<String>): Int =
        designs.count { designPossible(towels, it) }

    fun countTotalDesignArrangements(towels: List<String>, designs: List<String>): ULong = designs.sumOf { design ->
        val dp = mutableMapOf(0 to 1UL)
        (1..design.length).forEach { i ->
            dp[i] = towels.filter { design.takeLast(i).startsWith(it) }.sumOf { dp[i - it.length]!! }
        }
        dp[design.length]!!
    }

}

fun main() {
    LinenLayout().run {
        val (towels, designs) = parseInput(readInput())
        println(countPossibleDesigns(towels, designs))
        println(countTotalDesignArrangements(towels, designs))
    }
}
