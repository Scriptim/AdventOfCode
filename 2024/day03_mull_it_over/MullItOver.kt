package day03_mull_it_over

import java.io.File

class MullItOver {

    fun readInput(): String = File("src/day03_mull_it_over/input.txt").readText()

    fun runProgram(memory: String): Int {
        val mulInstructionRegex = Regex("""mul\((\d{1,3}),(\d{1,3})\)""")
        val instructions = mulInstructionRegex.findAll(memory).toList()
        return instructions.sumOf { it.groupValues[1].toInt() * it.groupValues[2].toInt() }
    }

}

fun main() {
    MullItOver().run {
        val memory = readInput()
        println(runProgram(memory))
    }
}
