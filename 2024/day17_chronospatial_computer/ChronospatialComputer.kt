package day17_chronospatial_computer

import java.io.File
import kotlin.math.pow

class ChronospatialComputer {

    fun readInput(): List<String> = File("src/day17_chronospatial_computer/input.txt").readLines()

    fun parseInput(lines: List<String>): Computer {
        val (a, b, c) = lines.take(3).map { it.split(": ")[1].toInt() }
        val program = lines[4].split(": ")[1].split(",").map { it.toInt() }
        return Computer(a, b, c, program)
    }

    data class Computer(var a: Int, var b: Int, var c: Int, val program: List<Int>, var instructionPointer: Int = 0) {

        val halted: Boolean
            get() = instructionPointer >= program.size

        val instruction: Int
            get() = program[instructionPointer]

        val operand: Int
            get() = program[instructionPointer + 1]

        val comboOperand: Int
            get() = when (operand) {
                4 -> a
                5 -> b
                6 -> c
                else -> operand
            }

    }

    fun runProgram(computer: Computer): String = computer.copy().run {
        val outputs = mutableListOf<Int>()

        while (!halted) {
            when (instruction) {
                0 -> a /= 2f.pow(comboOperand).toInt()
                1 -> b = b xor operand
                2 -> b = comboOperand % 8
                3 -> if (a != 0) instructionPointer = operand - 2
                4 -> b = b xor c
                5 -> outputs.add(comboOperand % 8)
                6 -> b = a / 2f.pow(comboOperand).toInt()
                7 -> c = a / 2f.pow(comboOperand).toInt()
            }

            instructionPointer += 2
        }

        return outputs.joinToString(",")
    }

}

fun main() {
    ChronospatialComputer().run {
        val computer = parseInput(readInput())
        println(runProgram(computer))
    }
}
