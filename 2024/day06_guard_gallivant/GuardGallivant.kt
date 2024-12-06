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

    fun countVisited(map: Map<Coordinate, MapTile>): Int {
        val visited = mutableSetOf<Coordinate>()
        val mutMap = map.toMutableMap()
        var position = mutMap.entries.first { it.value.isGuard() }.key

        while (position in mutMap) {
            visited.add(position)

            val newPosition = position + mutMap[position]!!.guardToDirection()

            when {
                newPosition !in mutMap -> {
                    mutMap[position] = MapTile.EMPTY
                    position = newPosition
                }

                mutMap[newPosition] == MapTile.OBSTRUCTION -> {
                    mutMap[position] = mutMap[position]!!.guardTurnRight()
                }

                else -> {
                    mutMap[newPosition] = mutMap[position]!!
                    mutMap[position] = MapTile.EMPTY
                    position = newPosition
                }
            }
        }

        return visited.size
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

        fun guardTurnRight(): MapTile = when (this) {
            GUARD_UP -> GUARD_RIGHT
            GUARD_RIGHT -> GUARD_DOWN
            GUARD_DOWN -> GUARD_LEFT
            GUARD_LEFT -> GUARD_UP
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
    }
}
