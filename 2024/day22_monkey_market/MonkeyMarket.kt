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

    private fun priceChanges(initialSecret: ULong): Sequence<Pair<Int, Int>> = sequence {
        var previousPrice = (initialSecret % 10UL).toInt()
        secretNumberSequence(initialSecret).forEach {
            val currentPrice = (it % 10UL).toInt()
            yield(Pair(currentPrice, currentPrice - previousPrice))
            previousPrice = currentPrice
        }
    }

    fun secretNumbersSum(secretNumbers: List<ULong>): ULong =
        secretNumbers.sumOf { secretNumberSequence(it).take(2000).last() }

    fun maximumBananas(secretNumbers: List<ULong>): Int {
        val bananas = mutableMapOf<List<Int>, Int>().withDefault { 0 }

        secretNumbers.forEach { seed ->
            val seen = mutableSetOf<List<Int>>()
            priceChanges(seed)
                .take(2000)
                .windowed(4)
                .map { changes -> changes.map { it.second } to changes.last().first }
                .forEach { (changes, price) ->
                    if (seen.add(changes)) bananas[changes] = bananas.getValue(changes) + price
                }
        }

        return bananas.values.max()
    }

}

fun main() {
    MonkeyMarket().run {
        val secretNumbers = parseInput(readInput())
        println(secretNumbersSum(secretNumbers))
        println(maximumBananas(secretNumbers))
    }
}
