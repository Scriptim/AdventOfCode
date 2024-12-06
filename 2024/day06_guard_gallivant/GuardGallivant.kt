package day06_guard_gallivant

import Coordinate
import Direction
import plus
import java.io.File

class GuardGallivant {

    fun readInput(): List<String> = File("src/day06_guard_gallivant/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Coordinate, MapTile> {
        val map = mutableMapOf<Coordinate, MapTile>()

        lines.forEachIndexed { row, line ->
            line.forEachIndexed { col, symbol ->
                map[Pair(col, row)] = MapTile.bySymbol(symbol)
            }
        }

        return map
    }

    private fun walk(map: Map<Coordinate, MapTile>, step: (Coordinate, Direction) -> Boolean) {
        var currentPosition = map.entries.first { it.value.isGuard() }.key
        var currentDirection = map[currentPosition]!!.guardToDirection()

        while (true) {
            while (map[currentPosition + currentDirection]?.takeIf { it != MapTile.OBSTRUCTION } != null) {
                if (!step(currentPosition, currentDirection)) return
                currentPosition += currentDirection
            }

            if (!step(currentPosition, currentDirection) || currentPosition + currentDirection !in map) return

            currentDirection = currentDirection.turnFullRight()
        }
    }

    private fun walkFull(map: Map<Coordinate, MapTile>, step: (Coordinate, Direction) -> Unit) =
        walk(map) { position, direction -> step(position, direction); true}

    fun countVisited(map: Map<Coordinate, MapTile>): Int = mutableSetOf<Coordinate>().run {
        walk(map) { position, _ -> add(position); true }
        size
    }

    fun countLoopObstructions(map: Map<Coordinate, MapTile>): Int {
        var count = 0
        val originalPath = mutableSetOf<Coordinate>()
        val mutMap = map.toMutableMap().apply {
            entries.filter { it.value.isGuard() }.forEach { this[it.key] = MapTile.EMPTY }
        }

        walkFull(map) { position, direction ->
            originalPath.add(position)
            val obstacle = position + direction

            if (map[obstacle] == MapTile.EMPTY && obstacle !in originalPath) {
                mutMap[position] = MapTile.byDirection(direction)
                mutMap[obstacle] = MapTile.OBSTRUCTION

                val path = mutableSetOf<Pair<Coordinate, Direction>>()
                walk(mutMap) { altPosition, altDirection ->
                    val loopy = !path.add(Pair(altPosition, altDirection))
                    if (loopy) count++
                    !loopy
                }

                mutMap[position] = MapTile.EMPTY
                mutMap[obstacle] = MapTile.EMPTY
            }
        }

        return count
    }

    enum class MapTile(val symbol: Char) {
        EMPTY('.'), OBSTRUCTION('#'), GUARD_UP('^'), GUARD_RIGHT('>'), GUARD_DOWN('v'), GUARD_LEFT('<');

        fun isGuard(): Boolean = this in GUARD_UP..GUARD_LEFT

        fun guardToDirection(): Direction = when (this) {
            GUARD_UP -> Direction.NORTH
            GUARD_RIGHT -> Direction.EAST
            GUARD_DOWN -> Direction.SOUTH
            GUARD_LEFT -> Direction.WEST
            else -> throw IllegalArgumentException("Not a guard tile")
        }

        companion object {
            fun bySymbol(symbol: Char): MapTile = entries.first { it.symbol == symbol }

            fun byDirection(direction: Direction): MapTile = when (direction) {
                Direction.NORTH -> GUARD_UP
                Direction.EAST -> GUARD_RIGHT
                Direction.SOUTH -> GUARD_DOWN
                Direction.WEST -> GUARD_LEFT
                else -> throw IllegalArgumentException("Not a guard direction")
            }
        }

    }

}

fun main() {
    GuardGallivant().run {
        val map = parseInput(readInput())
        println(countVisited(map))
        println(countLoopObstructions(map))
    }
}
