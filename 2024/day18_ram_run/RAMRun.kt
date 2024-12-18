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

    fun shortestPathAfterKilobyte(corruptBytes: List<Vector2D<Int>>): Int {
        val entry = Vector2D(0, 0)
        val exit = Vector2D(width - 1, height - 1)

        val corruptKilobyte = corruptBytes.take(1024).toSet()

        val queue = PriorityQueue<Pair<Vector2D<Int>, Int>>(compareBy { it.second }).apply { add(entry to 0) }
        val steps = mutableMapOf(entry to 0).withDefault { Int.MAX_VALUE }

        while (queue.isNotEmpty()) {
            val (current, _) = queue.remove()
            if (current == exit) return steps.getValue(current)

            Direction.VON_NEUMANN_NEIGHBORHOOD
                .map { current + it }
                .filter { it.x in 0..<width && it.y in 0..<height && it !in corruptKilobyte }
                .forEach { neighbor ->
                    val newSteps = steps.getValue(current) + 1
                    if (newSteps < steps.getValue(neighbor)) {
                        steps[neighbor] = newSteps
                        queue.add(neighbor to newSteps + (exit - neighbor).manhattan())
                    }
                }
        }

        throw IllegalArgumentException("No path found")
    }

}

fun main() {
    RAMRun().run {
        val corruptBytes = parseInput(readInput())
        println(shortestPathAfterKilobyte(corruptBytes))
    }
}
