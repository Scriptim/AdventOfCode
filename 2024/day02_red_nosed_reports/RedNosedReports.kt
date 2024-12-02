package day02_red_nosed_reports

import java.io.File

typealias Report = List<Int>

class RedNosedReports {

    fun readInput(): List<String> = File("src/day02_red_nosed_reports/input.txt").readLines()

    fun parseInput(lines: List<String>): List<Report> = lines.map { it.split(" ").map(String::toInt) }

    fun numSafe(reports: List<Report>): Int = reports.count { report ->
        val deltas: List<Int> = report.indices.drop(1).map { i -> report[i] - report[i - 1] }
        deltas.all { it in -3..-1 } || deltas.all { it in 1..3 }
    }

}

fun main() {
    RedNosedReports().run {
        val reports = parseInput(readInput())
        println(numSafe(reports))
    }
}
