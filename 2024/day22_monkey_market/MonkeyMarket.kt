package day22_monkey_market

import java.io.File

class MonkeyMarket {

    fun readInput(): List<String> = File("src/day22_monkey_market/input.txt").readLines()

    fun parseInput(lines: List<String>): List<ULong> = lines.map { it.toULong() }

    private fun secretNumberSequence(initialSecret: ULong): Sequence<ULong> = sequence {
        var currentSecret = initialSecret
        while (true) {
            currentSecret = currentSecret xor (currentSecret shl 6) and 0xFFFFFFUL
            currentSecret = currentSecret xor (currentSecret shr 5) and 0xFFFFFFUL
            currentSecret = currentSecret xor (currentSecret shl 11) and 0xFFFFFFUL
            yield(currentSecret)
        }
    }

    fun secretNumbersSum(secretNumbers: List<ULong>): ULong =
        secretNumbers.sumOf { secretNumberSequence(it).take(2000).last() }

}

fun main() {
    MonkeyMarket().run {
        val secretNumbers = parseInput(readInput())
        println(secretNumbersSum(secretNumbers))
    }
}
