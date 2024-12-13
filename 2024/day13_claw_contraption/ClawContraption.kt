package day13_claw_contraption

import Vector2D
import plus
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
            Vector2D(xButtonA.toLong(), yButtonA.toLong()),
            Vector2D(xButtonB.toLong(), yButtonB.toLong()),
            Vector2D(xPrize.toLong(), yPrize.toLong())
        )
    }.toSet()
}

data class ClawMachine(val buttonA: Vector2D<Long>, val buttonB: Vector2D<Long>, val prize: Vector2D<Long>)

fun totalCost(machines: Set<ClawMachine>, moveFurther: Boolean = false): Long = machines.sumOf { it.run {
    val prize = if (moveFurther) prize + Vector2D(10000000000000L, 10000000000000L) else prize

    val determinant = buttonA.x * buttonB.y - buttonA.y * buttonB.x; require(determinant != 0L)
    val aPresses = (buttonB.y * prize.x - buttonB.x * prize.y)
    val bPresses = (buttonA.x * prize.y - buttonA.y * prize.x)

    if (aPresses % determinant != 0L || bPresses % determinant != 0L) 0L else (3L * aPresses + bPresses) / determinant
}}

fun main() {
    ClawContraption().run {
        val machines = parseInput(readInput())
        println(totalCost(machines))
        println(totalCost(machines, moveFurther = true))
    }
}
