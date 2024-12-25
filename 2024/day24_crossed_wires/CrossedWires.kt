package day24_crossed_wires

import java.io.File
import java.util.*
import kotlin.collections.ArrayDeque

class CrossedWires {

    fun readInput(): List<String> = File("src/day24_crossed_wires/input.txt").readLines()

    fun parseInput(lines: List<String>): Pair<Map<String, Boolean>, Set<LogicGate>> {
        val wires = lines.takeWhile { it.isNotBlank() }.associate {
            val (name, value) = it.split(": ")
            name to (value == "1")
        }
        val gates = lines.drop(wires.size + 1).map {
            val (input1, operator, input2, _, output) = it.split(" ")
            LogicGate(input1, input2, BooleanOperator.valueOf(operator), output)
        }.toSet()
        return Pair(wires, gates)
    }

    data class LogicGate(val input1: String, val input2: String, val operator: BooleanOperator, val output: String) {

        fun evaluate(values: Map<String, Boolean>): Boolean = operator.evaluate(values[input1]!!, values[input2]!!)

        fun isInput(): Boolean = input1.first() in "xy" || input2.first() in "xy"

        fun isOutput(): Boolean = output.startsWith('z')

    }

    enum class BooleanOperator(val evaluate: (Boolean, Boolean) -> Boolean) {
        AND(Boolean::and), OR(Boolean::or), XOR(Boolean::xor)
    }

    private fun evaluate(wireInputs: Map<String, Boolean>, gates: Set<LogicGate>): Map<String, Boolean> {
        val finalWires = wireInputs.toMutableMap()
        val gateQueue = ArrayDeque(gates)

        while (gateQueue.isNotEmpty()) {
            gateQueue.removeFirst().apply {
                if ((input1 in finalWires) and (input2 in finalWires)) {
                    finalWires[output] = evaluate(finalWires)
                } else {
                    gateQueue.addLast(this)
                }
            }
        }

        return finalWires.toMap()
    }

    fun numberOutput(wireInputs: Map<String, Boolean>, gates: Set<LogicGate>): ULong = evaluate(wireInputs, gates)
        .filterKeys { it.startsWith('z') }
        .toSortedMap(Collections.reverseOrder()).values
        .fold(0UL) { acc, value -> (acc shl 1) + if (value) 1UL else 0UL }

    fun swappedGates(gates: Set<LogicGate>): String {
        fun bit(wire: String): Int = wire.drop(1).toInt()
        fun LogicGate.successors(): Set<LogicGate> = gates.filter { it.input1 == output || it.input2 == output }.toSet()
        val msb = gates.filter { it.isOutput() }.maxOf { bit(it.output) }

        return gates.filterNot { gate ->
            gate.run {
                when {
                    isOutput() -> bit(output) == msb || operator == BooleanOperator.XOR
                    operator == BooleanOperator.XOR -> isInput() && successors().all {
                        it.operator in setOf(BooleanOperator.AND, BooleanOperator.XOR)
                    }
                    operator == BooleanOperator.AND -> (isInput() && bit(input1) == 0) || successors().all {
                        it.operator == BooleanOperator.OR
                    }
                    else -> true
                }
            }
        }.map { it.output }.sorted().joinToString(",")
    }

}

fun main() {
    CrossedWires().run {
        val (wireInputs, gates) = parseInput(readInput())
        println(numberOutput(wireInputs, gates))
        println(swappedGates(gates))
    }
}
