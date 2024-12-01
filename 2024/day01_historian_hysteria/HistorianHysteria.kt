package day01_historian_hysteria

import toPair
import java.io.File
import kotlin.math.abs

class HistorianHysteria {

    fun readInput(): List<String> =
        File("src/day01_historian_hysteria/input.txt").readLines()

    fun parseInput(lines: List<String>): Pair<List<Int>, List<Int>> =
        lines.map { it.split("   ").map(String::toInt).toPair() }.unzip()

    fun totalListDistance(left: List<Int>, right: List<Int>): Int =
        left.sorted().zip(right.sorted()).sumOf { (l, r) -> abs(l - r) }

    fun totalSimilarityScore(left: List<Int>, right: List<Int>): Int =
        left.sumOf { l -> l * right.count { r -> r == l } }

}

fun main() {
    HistorianHysteria().run {
        val (left, right) = parseInput(readInput())
        println(totalListDistance(left, right))
        println(totalSimilarityScore(left, right))
    }
}
