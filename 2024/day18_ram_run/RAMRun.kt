package day18_ram_run

import Direction
import Vector2D
import manhattan
import minus
import plus
import java.io.File
import java.util.*

class RAMRun {

    private val width = 71
    private val height = 71

    fun readInput(): List<String> = File("src/day18_ram_run/input.txt").readLines()

    fun parseInput(lines: List<String>): List<Vector2D<Int>> = lines.map { line ->
        val (x, y) = line.split(",").map { it.toInt() }
        Vector2D(x, y)
    }

    private fun shortestPath(corruptBytes: Set<Vector2D<Int>>): Int? {
        val entry = Vector2D(0, 0)
        val exit = Vector2D(width - 1, height - 1)

        val queue = PriorityQueue<Pair<Vector2D<Int>, Int>>(compareBy { it.second }).apply { add(entry to 0) }
        val steps = mutableMapOf(entry to 0).withDefault { Int.MAX_VALUE }

        while (queue.isNotEmpty()) {
            val (current, _) = queue.remove()
            if (current == exit) return steps.getValue(current)

            Direction.VON_NEUMANN_NEIGHBORHOOD
                .map { current + it }
                .filter { it.x in 0..<width && it.y in 0..<height && it !in corruptBytes }
                .forEach { neighbor ->
                    val newSteps = steps.getValue(current) + 1
                    if (newSteps < steps.getValue(neighbor)) {
                        steps[neighbor] = newSteps
                        queue.add(neighbor to newSteps + (exit - neighbor).manhattan())
                    }
                }
        }

        return null
    }

    fun shortestPathAfterKilobyte(corruptBytes: List<Vector2D<Int>>): Int =
        shortestPath(corruptBytes.take(1024).toSet())!!

    fun cutOffByte(corruptBytes: List<Vector2D<Int>>): String {
        var lowerBound = 0
        var upperBound = corruptBytes.size - 1

        while (lowerBound < upperBound) {
            val cutOff = (lowerBound + upperBound) / 2
            val path = shortestPath(corruptBytes.take(cutOff).toSet())
            if (path != null) lowerBound = cutOff else upperBound = cutOff - 1
        }

        return "${corruptBytes[upperBound].x},${corruptBytes[upperBound].y}"
    }

}

fun main() {
    RAMRun().run {
        val corruptBytes = parseInput(readInput())
        println(shortestPathAfterKilobyte(corruptBytes))
        println(cutOffByte(corruptBytes))
    }
}
