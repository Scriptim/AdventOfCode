package day05_print_queue

import toPair
import java.io.File

typealias OrderingRule = Pair<Int, Int>
typealias Pages = List<Int>

class PrintQueue {

    fun readInput(): List<String> = File("src/day05_print_queue/input.txt").readLines()

    fun parseInput(lines: List<String>): Pair<List<OrderingRule>, List<Pages>> {
        val separatorLine = lines.indexOfFirst { it.isEmpty() }
        val orderingRules = lines.take(separatorLine).map { it.split("|").map(String::toInt).toPair() }
        val pages = lines.drop(separatorLine + 1).map { it.split(",").map(String::toInt) }
        return Pair(orderingRules, pages)
    }

    private fun isOrdered(orderingRules: List<OrderingRule>, pages: Pages): Boolean = orderingRules.all {
        val (a, b) = it
        a !in pages || b !in pages || pages.indexOf(a) < pages.indexOf(b)
    }

    fun validMiddlePageSum(orderingRules: List<OrderingRule>, updates: List<Pages>): Int =
        updates.filter { isOrdered(orderingRules, it) }.sumOf { it[it.size / 2] }

}

fun main() {
    PrintQueue().run {
        val (orderingRules, pages) = parseInput(readInput())
        println(validMiddlePageSum(orderingRules, pages))
    }
}
