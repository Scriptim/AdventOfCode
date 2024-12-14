package day14_restroom_redoubt

import Vector2D
import div
import minus
import plus
import java.io.File

class RestroomRedoubt {

    private val width = 101
    private val height = 103
    private val robotRegex = Regex("""p=(\d+),(\d+) v=(-?\d+),(-?\d+)""")

    fun readInput(): List<String> = File("src/day14_restroom_redoubt/input.txt").readLines()

    fun parseInput(lines: List<String>): Set<Robot> = lines.map {
        val (xPosition, yPosition, xVelocity, yVelocity) = robotRegex.matchEntire(it)!!.destructured
        Robot(Vector2D(xPosition.toInt(), yPosition.toInt()), Vector2D(xVelocity.toInt(), yVelocity.toInt()))
    }.toSet()

    data class Robot(val position: Vector2D<Int>, val velocity: Vector2D<Int>)

    private fun move(robot: Robot, seconds: Int): Robot = robot.copy(
        position = Vector2D(
            (robot.position.x + robot.velocity.x * seconds).mod(width),
            (robot.position.y + robot.velocity.y * seconds).mod(height)
        )
    )

    fun safetyFactor(robots: Set<Robot>): Int {
        val finalRobotsPosition = robots.map { robot -> move(robot, 100) }
        return finalRobotsPosition.count { it.position.x < width / 2 && it.position.y < height / 2 } *
               finalRobotsPosition.count { it.position.x < width / 2 && it.position.y > height / 2 } *
               finalRobotsPosition.count { it.position.x > width / 2 && it.position.y > height / 2 } *
               finalRobotsPosition.count { it.position.x > width / 2 && it.position.y < height / 2 }
    }

    fun findChristmasTree(robots: Set<Robot>): Int = (0..<width * height).minBy { seconds ->
        val finalRobotsPosition = robots.map { robot -> move(robot, seconds) }
        val expected = finalRobotsPosition.map { it.position }.reduce { acc, position -> acc + position } / robots.size
        val variance = finalRobotsPosition.sumOf { robot -> (robot.position - expected).run { x * x + y * y } }
        variance
    }

}

fun main() {
    RestroomRedoubt().run {
        val robots = parseInput(readInput())
        println(safetyFactor(robots))
        println(findChristmasTree(robots))
    }
}

