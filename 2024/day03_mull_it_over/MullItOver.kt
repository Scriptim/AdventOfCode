package day03_mull_it_over

import java.io.File

class MullItOver {

    private val mulInstructionRegex = Regex("""mul\((\d{1,3}),(\d{1,3})\)""")
    private val doInstructionRegex = Regex("""do(?!n't)\(\)""")
    private val doNotInstructionRegex = Regex("""don't\(\)""")

    fun readInput(): String = File("src/day03_mull_it_over/input.txt").readText()

    private fun evalMulInstruction(instruction: MatchResult): Int =
        instruction.groupValues[1].toInt() * instruction.groupValues[2].toInt()

    fun runProgram(memory: String): Int = mulInstructionRegex.findAll(memory).map(::evalMulInstruction).sum()

    fun runProgramConditionally(memory: String): Int {
        var sum = 0
        var enabled = true
        memory.indices.forEach {
            when {
                doInstructionRegex.matchesAt(memory, it) -> enabled = true
                doNotInstructionRegex.matchesAt(memory, it) -> enabled = false
                enabled && mulInstructionRegex.matchesAt(memory, it) ->
                    sum += evalMulInstruction(mulInstructionRegex.find(memory, it)!!)
            }
        }

        return sum
    }

}

fun main() {
    MullItOver().run {
        val memory = readInput()
        println(runProgram(memory))
        println(runProgramConditionally(memory))
    }
}
