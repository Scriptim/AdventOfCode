package day17_chronospatial_computer

import java.io.File

class ChronospatialComputer {

    fun readInput(): List<String> = File("src/day17_chronospatial_computer/input.txt").readLines()

    fun parseInput(lines: List<String>): Computer {
        val (a, b, c) = lines.take(3).map { it.split(": ")[1].toULong() }
        val program = lines[4].split(": ")[1].split(",").map { it.toInt() }
        return Computer(a, b, c, program)
    }

    data class Computer(
        var a: ULong, var b: ULong, var c: ULong, val program: List<Int>, var instructionPointer: Int = 0
    ) {

        val halted: Boolean
            get() = instructionPointer >= program.size

        val instruction: Int
            get() = program[instructionPointer]

        val operand: ULong
            get() = program[instructionPointer + 1].toULong()

        val comboOperand: ULong
            get() = when (operand) {
                4UL -> a
                5UL -> b
                6UL -> c
                else -> operand
            }

    }

    private fun runProgram(computer: Computer): Sequence<Int> = sequence {
        computer.run {
            while (!halted) {
                when (instruction) {
                    0 -> a = a shr comboOperand.toInt()
                    1 -> b = b xor operand
                    2 -> b = comboOperand and 7UL
                    3 -> if (a > 0UL) instructionPointer = operand.toInt() - 2
                    4 -> b = b xor c
                    5 -> yield((comboOperand and 7UL).toInt())
                    6 -> b = a shr comboOperand.toInt()
                    7 -> c = a shr comboOperand.toInt()
                }

                instructionPointer += 2
            }
        }
    }

    fun programOutput(computer: Computer): String = runProgram(computer.copy()).joinToString(",")

    fun findInitialAValue(computer: Computer): ULong {
        fun dfs(outputIndex: Int, expectedA: ULong): ULong? {
            if (outputIndex < 0) return expectedA

            val expectedOutput = computer.program[outputIndex]

            return (expectedA * 8UL..<(expectedA + 1UL) * 8UL).firstNotNullOfOrNull { a ->
                val simulateComputer = computer.copy(a = a)
                val actualOutput = runProgram(simulateComputer).first()
                val actualA = simulateComputer.a

                if (actualA == expectedA && actualOutput == expectedOutput) {
                    dfs(outputIndex - 1, a)
                } else null
            }
        }

        return dfs(computer.program.size - 1, 0UL)!!
    }

}

fun main() {
    ChronospatialComputer().run {
        val computer = parseInput(readInput())
        println(programOutput(computer))
        println(findInitialAValue(computer))
    }
}
