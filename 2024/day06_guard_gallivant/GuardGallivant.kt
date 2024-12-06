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

    private fun path(map: Map<Coordinate, MapTile>): List<Pair<Coordinate, Direction>> {
        val path = mutableListOf<Pair<Coordinate, Direction>>()
        var currentPosition = map.entries.first { it.value.isGuard() }.key
        var currentDirection = map[currentPosition]!!.guardToDirection()

        while (true) {
            while (map[currentPosition + currentDirection]?.takeIf { it != MapTile.OBSTRUCTION } != null) {
                path.add(Pair(currentPosition, currentDirection))
                currentPosition += currentDirection
                if (Pair(currentPosition, currentDirection) in path) return path.toList()
            }

            path.add(Pair(currentPosition, currentDirection))
            if (currentPosition + currentDirection !in map) return path.toList()
            currentDirection = currentDirection.turnFullRight()
        }
    }

    fun countVisited(map: Map<Coordinate, MapTile>): Int = path(map).map { it.first }.toSet().size

    fun countLoopObstructions(map: Map<Coordinate, MapTile>): Int {
        val path = path(map)
        val mutMap = map.toMutableMap().apply {
            entries.filter { it.value.isGuard() }.forEach { this[it.key] = MapTile.EMPTY }
        }

        return path.withIndex().count { (index, pathElement) ->
            val (position, direction) = pathElement
            val obstacle = position + direction

            if (map[obstacle] != MapTile.EMPTY || obstacle in path.take(index).map { it.first }) {
                return@count false
            }

            mutMap[position] = when (direction) {
                Direction.NORTH -> MapTile.GUARD_UP
                Direction.EAST -> MapTile.GUARD_RIGHT
                Direction.SOUTH -> MapTile.GUARD_DOWN
                Direction.WEST -> MapTile.GUARD_LEFT
                else -> throw IllegalArgumentException("Not a guard direction")
            }
            mutMap[obstacle] = MapTile.OBSTRUCTION

            val loopy = path(mutMap).last().let { it.first + it.second } in map

            mutMap[position] = MapTile.EMPTY
            mutMap[obstacle] = MapTile.EMPTY

            loopy
        }
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
