package day05_print_queue

import toPair
import java.io.File

typealias OrderingRule = Pair<Int, Int>
typealias Pages = List<Int>

class PrintQueue {

    fun readInput(): List<String> = File("src/day05_print_queue/input.txt").readLines()

    fun parseInput(lines: List<String>): Pair<Set<OrderingRule>, List<Pages>> {
        val separatorLine = lines.indexOfFirst { it.isEmpty() }
        val orderingRules = lines.take(separatorLine).map { it.split("|").map(String::toInt).toPair() }.toSet()
        val pages = lines.drop(separatorLine + 1).map { it.split(",").map(String::toInt) }
        return Pair(orderingRules, pages)
    }

    private fun isOrdered(orderingRules: Set<OrderingRule>, pages: Pages): Boolean = orderingRules.all {
        val (a, b) = it
        a !in pages || b !in pages || pages.indexOf(a) < pages.indexOf(b)
    }

    private fun lowestPages(orderingRules: Set<OrderingRule>): Set<Int> =
        orderingRules.map { it.first }.filter { page -> orderingRules.none { it.second == page } }.toSet()

    private fun topologicalOrder(orderingRules: Set<OrderingRule>): Pages {
        val sortedPages = mutableListOf<Int>()
        val remainingRules = orderingRules.toMutableSet()
        val queue = ArrayDeque(lowestPages(orderingRules))

        while (queue.isNotEmpty()) {
            val page = queue.removeFirst()
            sortedPages.add(page)

            val successors = remainingRules.filter { it.first == page }.map { it.second }
            remainingRules.removeAll { it.first == page }

            queue.addAll(successors.filter { successor -> remainingRules.none { it.second == successor } })
        }

        return sortedPages.toList()
    }

    fun validMiddlePageSum(orderingRules: Set<OrderingRule>, updates: List<Pages>): Int =
        updates.filter { isOrdered(orderingRules, it) }.sumOf { it[it.size / 2] }

    fun invalidMiddlePageSum(orderingRules: Set<OrderingRule>, updates: List<Pages>): Int = updates
        .filterNot { isOrdered(orderingRules, it) }
        .map { pages -> topologicalOrder(orderingRules.filter { it.first in pages && it.second in pages }.toSet()) }
        .sumOf { it[it.size / 2] }

}

fun main() {
    PrintQueue().run {
        val (orderingRules, pages) = parseInput(readInput())
        println(validMiddlePageSum(orderingRules, pages))
        println(invalidMiddlePageSum(orderingRules, pages))
    }
}
