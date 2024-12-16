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

    private fun pathScores(maze: Map<Vector2D<Int>, Char>): Map<Vector2D<Int>, Map<Direction, UInt>> {
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

        return scores.mapValues { it.value.toMap() }.toMap()
    }

    private fun scoresAfterTurn(
        pathScores: Map<Vector2D<Int>, Map<Direction, UInt>>, position: Vector2D<Int>, direction: Direction
    ): Map<Direction, UInt> =
        Direction.VON_NEUMANN_NEIGHBORHOOD
        .filter { it in pathScores[position]!! }
        .associateWith {
            pathScores[position]!![it]!! + 1000U * when (it) {
                direction -> 0U
                direction.turnAround() -> 2U
                else -> 1U
            }
        }

    fun lowestPathScore(maze: Map<Vector2D<Int>, Char>): UInt =
        scoresAfterTurn(pathScores(maze), maze.filterValues { it == 'S' }.keys.first(), Direction.EAST).values.min()

    fun optimalPathsTiles(maze: Map<Vector2D<Int>, Char>): Int {
        val start = maze.filterValues { it == 'S' }.keys.first()
        val pathScores = pathScores(maze)
        val tiles = mutableSetOf<Vector2D<Int>>()

        fun dfs(current: Vector2D<Int>, direction: Direction) {
            if (maze[current] == '#') return
            tiles.add(current)
            val scores = scoresAfterTurn(pathScores, current, direction)
            val optimalScore = scores.values.minOrNull() ?: return
            scores.filterValues { it == optimalScore }.forEach { dfs(current + it.key, it.key) }
        }
        dfs(start, Direction.EAST)

        return tiles.size
    }

}

fun main() {
    ReindeerMaze().run {
        val maze = parseInput(readInput())
        println(lowestPathScore(maze))
        println(optimalPathsTiles(maze))
    }
}
