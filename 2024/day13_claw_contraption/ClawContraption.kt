package day13_claw_contraption

import Vector2D
import java.io.File

class ClawContraption {

    private val buttonRegex = Regex("""Button [AB]: X\+(\d+), Y\+(\d+)""")
    private val prizeRegex = Regex("""Prize: X=(\d+), Y=(\d+)""")

    fun readInput(): List<String> = File("src/day13_claw_contraption/input.txt").readLines()

    fun parseInput(lines: List<String>): Set<ClawMachine> = lines.chunked(4).map { machineLines ->
        val (xButtonA, yButtonA) = buttonRegex.matchEntire(machineLines[0])!!.destructured
        val (xButtonB, yButtonB) = buttonRegex.matchEntire(machineLines[1])!!.destructured
        val (xPrize, yPrize) = prizeRegex.matchEntire(machineLines[2])!!.destructured

        ClawMachine(
            Vector2D(xButtonA.toInt(), yButtonA.toInt()),
            Vector2D(xButtonB.toInt(), yButtonB.toInt()),
            Vector2D(xPrize.toInt(), yPrize.toInt())
        )
    }.toSet()
}

data class ClawMachine(val buttonA: Vector2D<Int>, val buttonB: Vector2D<Int>, val prize: Vector2D<Int>)

fun totalCost(machines: Set<ClawMachine>): Int = machines.sumOf {
    it.run {
        val determinant = buttonA.x * buttonB.y - buttonA.y * buttonB.x
        val aPresses = (buttonB.y * prize.x - buttonB.x * prize.y) / determinant
        val bPresses = (buttonA.x * prize.y - buttonA.y * prize.x) / determinant

        if ((buttonB.y * prize.x - buttonB.x * prize.y) % determinant != 0) 0 else 3 * aPresses + bPresses
    }
}

fun main() {
    ClawContraption().run {
        val machines = parseInput(readInput())
        println(totalCost(machines))
    }
}
