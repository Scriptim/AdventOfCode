package day16_reindeer_maze

import Direction
import Vector2D
import parse2DMap
import plus
import java.io.File
import java.util.*

class ReindeerMaze {

    fun readInput(): List<String> = File("src/day16_reindeer_maze/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Vector2D<Int>, Char> = parse2DMap(lines)

    fun lowestPathScore(maze: Map<Vector2D<Int>, Char>): UInt {
        val start = maze.filterValues { it == 'S' }.keys.first()
        val end = maze.filterValues { it == 'E' }.keys.first()

        val scores = maze.keys.associateWith { mutableMapOf<Direction, UInt>() }.toMutableMap()
        Direction.VON_NEUMANN_NEIGHBORHOOD.forEach { scores[end]!![it] = 0U }

        val queue = ArrayDeque<Vector2D<Int>>().apply { add(end) }

        while (queue.isNotEmpty()) {
            val current = queue.removeFirst()

            Direction.VON_NEUMANN_NEIGHBORHOOD.forEach { direction ->
                val predecessor = current + direction.turnAround()
                if (maze[predecessor] == '#') return@forEach

                val newScore = Direction.VON_NEUMANN_NEIGHBORHOOD.minOf { nextDirection ->
                    scores[current]!![nextDirection]?.let {
                        it + 1U + 1000U * when (direction) {
                            nextDirection -> 0U
                            direction.turnAround() -> 2U
                            else -> 1U
                        }
                    } ?: UInt.MAX_VALUE
                }

                if (newScore < (scores[predecessor]!![direction] ?: UInt.MAX_VALUE)) {
                    scores[predecessor]!![direction] = newScore
                    queue.addLast(predecessor)
                }
            }
        }

        return scores[start]!!.minOf {
            it.value + 1000U * when (it.key) {
                Direction.EAST -> 0U
                Direction.WEST -> 2U
                else -> 1U
            }
        }
    }

}

fun main() {
    ReindeerMaze().run {
        val maze = parseInput(readInput())
        println(lowestPathScore(maze))
    }
}
