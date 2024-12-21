package day21_keypad_conundrum

import Vector2D
import minus
import parse2DMap
import plus
import java.io.File
import kotlin.math.abs

class KeypadConundrum {

    private val numericKeypad = parse2DMap(listOf("#0A", "123", "456", "789")).filterValues { it != '#' }
    private val directionalKeypad = parse2DMap(listOf("<v>", "#^A")).filterValues { it != '#' }

    fun readInput(): List<String> = File("src/day21_keypad_conundrum/input.txt").readLines()

    private fun buttonPaths(current: Char, target: Char, keypad: Map<Vector2D<Int>, Char>): Set<String> {
        val currentPosition = keypad.filterValues { it == current }.keys.first()
        val targetPosition = keypad.filterValues { it == target }.keys.first()
        val delta = targetPosition - currentPosition

        val horizontal = (if (delta.x > 0) ">" else "<").repeat(abs(delta.x))
        val vertical = (if (delta.y > 0) "^" else "v").repeat(abs(delta.y))

        return setOfNotNull(
            currentPosition.takeIf { it + Vector2D(delta.x, 0) in keypad }?.let { horizontal + vertical + 'A' },
            currentPosition.takeIf { it + Vector2D(0, delta.y) in keypad }?.let { vertical + horizontal + 'A' })
    }

    private fun shortestButtonSequence(code: String, numIndirections: Int): ULong {
        val memo = mutableMapOf<Pair<String, Int>, ULong>()
        fun dp(code: String, indirection: Int): ULong {
            if (indirection == 0) return code.length.toULong()
            return memo.getOrPut(Pair(code, indirection)) {
                val keypad = if (indirection == numIndirections) numericKeypad else directionalKeypad
                code.fold(Pair('A', 0UL)) { (current, value), next ->
                    Pair(next, value + buttonPaths(current, next, keypad).minOf { dp(it, indirection - 1) })
                }.second
            }
        }

        return dp(code, numIndirections)
    }

    fun codeComplexitySum(codes: List<String>, numIndirections: Int) = codes.sumOf { code ->
        shortestButtonSequence(code, numIndirections) * code.filter { it.isDigit() }.toULong(10)
    }

}

fun main() {
    KeypadConundrum().run {
        val codes = readInput()
        println(codeComplexitySum(codes, numIndirections = 3))
        println(codeComplexitySum(codes, numIndirections = 26))
    }
}
