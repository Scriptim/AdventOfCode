package day04_ceres_search

import java.io.File

class CeresSearch {

    fun readInput(): List<String> = File("src/day04_ceres_search/input.txt").readLines()

    private fun readStringAt(
        wordSearch: List<String>, position: Pair<Int, Int>, direction: Direction, length: Int
    ): String = buildString {
        var current = position
        repeat(length) {
            val (row, col) = current
            if (row in wordSearch.indices && col in wordSearch[row].indices) {
                append(wordSearch[row][col])
                current += direction
            }
        }
    }

    fun countWord(wordSearch: List<String>, word: String): Int {
        var count = 0

        wordSearch.forEachIndexed { row, line ->
            line.forEachIndexed { col, _ ->
                count += Direction.entries.count {
                    readStringAt(wordSearch, Pair(row, col), it, word.length) == word
                }
            }
        }

        return count
    }

    private enum class Direction(val delta: Pair<Int, Int>) {
        NORTH(Pair(0, -1)),
        NORTH_EAST(Pair(1, -1)),
        EAST(Pair(1, 0)),
        SOUTH_EAST(Pair(1, 1)),
        SOUTH(Pair(0, 1)),
        SOUTH_WEST(Pair(-1, 1)),
        WEST(Pair(-1, 0)),
        NORTH_WEST(Pair(-1, -1))
    }

    private operator fun Pair<Int, Int>.plus(direction: Direction): Pair<Int, Int> =
        Pair(first + direction.delta.first, second + direction.delta.second)

}

fun main() {
    CeresSearch().run {
        val wordSearch = readInput()
        println(countWord(wordSearch, "XMAS"))
    }
}
