package day07_bridge_repair

import java.io.File

class BridgeRepair {

    fun readInput(): List<String> = File("src/day07_bridge_repair/input.txt").readLines()

    fun parseInput(lines: List<String>): List<Pair<ULong, List<ULong>>> = lines.map {
        val (testValue, operands) = it.split(": ")
        testValue.toULong() to operands.split(" ").map(String::toULong)
    }

    private fun isSatisfiable(testValue: ULong, operands: List<ULong>, accu: ULong, index: Int): Boolean {
        if (accu > testValue) return false
        if (index == operands.size) return accu == testValue

        return isSatisfiable(testValue, operands, accu + operands[index], index + 1) ||
               isSatisfiable(testValue, operands, accu * operands[index], index + 1)
    }

    fun totalCalibrationResult(equations: List<Pair<ULong, List<ULong>>>): ULong =
        equations.filter { isSatisfiable(it.first, it.second, it.second.first(), 1) }.sumOf { it.first }

}

fun main() {
    BridgeRepair().run {
        val equations = parseInput(readInput())
        println(totalCalibrationResult(equations))
    }
}

