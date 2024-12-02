package day02_red_nosed_reports

import dropAt
import java.io.File
import kotlin.math.abs

typealias Report = List<Int>

class RedNosedReports {

    fun readInput(): List<String> = File("src/day02_red_nosed_reports/input.txt").readLines()

    fun parseInput(lines: List<String>): List<Report> = lines.map { it.split(" ").map(String::toInt) }

    private fun isSafe(report: Report, problemDampener: Boolean = false): Boolean {
        val deltas: MutableList<Int> = report.indices.drop(1).map { i -> report[i] - report[i - 1] }.toMutableList()
        val shouldIncrease = deltas.count { it > 0 } > deltas.count { it < 0 }

        return deltas.withIndex().all { (i, d) ->
            when {
                abs(d) in 1..3 && (d > 0 == shouldIncrease) -> true
                problemDampener -> isSafe(report.dropAt(i)) || isSafe(report.dropAt(i + 1))
                else -> false
            }
        }
    }

    fun numSafe(reports: List<Report>, problemDampener: Boolean): Int = reports.count { isSafe(it, problemDampener) }

}

fun main() {
    RedNosedReports().run {
        val reports = parseInput(readInput())
        println(numSafe(reports, false))
        println(numSafe(reports, true))
    }
}
