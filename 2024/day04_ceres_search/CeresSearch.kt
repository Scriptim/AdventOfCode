package day04_ceres_search

import Direction
import Vector2D
import minus
import plus
import java.io.File

class CeresSearch {

    fun readInput(): List<String> = File("src/day04_ceres_search/input.txt").readLines()

    private fun readStringAt(
        wordSearch: List<String>, position: Vector2D<Int>, direction: Direction, length: Int
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
                    readStringAt(wordSearch, Vector2D(row, col), it, word.length) == word
                }
            }
        }

        return count
    }

    fun countWordCross(wordSearch: List<String>, word: String): Int {
        var count = 0

        wordSearch.forEachIndexed { row, line ->
            count += line.indices.count { col ->
                fun hasDiagonalWord(direction: Direction) =
                    readStringAt(wordSearch, Vector2D(row, col) - direction, direction, word.length) == word

                (hasDiagonalWord(Direction.SOUTH_EAST) || hasDiagonalWord(Direction.NORTH_WEST)) &&
                (hasDiagonalWord(Direction.SOUTH_WEST) || hasDiagonalWord(Direction.NORTH_EAST))
            }
        }

        return count
    }

}

fun main() {
    CeresSearch().run {
        val wordSearch = readInput()
        println(countWord(wordSearch, "XMAS"))
        println(countWordCross(wordSearch, "MAS"))
    }
}
