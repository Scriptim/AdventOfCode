package day12_garden_groups

import Coordinate
import Direction
import plus
import java.io.File

class GardenGroups {

    fun readInput(): List<String> = File("src/day12_garden_groups/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Coordinate, Char> = lines.flatMapIndexed { row, line ->
        line.mapIndexed { column, char -> Pair(column, row) to char }
    }.toMap()

    private fun regions(map: Map<Coordinate, Char>): Set<Set<Coordinate>> {
        val visited = mutableSetOf<Coordinate>()
        val regions = mutableSetOf<MutableSet<Coordinate>>()

        fun flood(coordinate: Coordinate, plant: Char, region: MutableSet<Coordinate>) {
            if (coordinate in visited) return
            visited.add(coordinate)

            Direction.VON_NEUMANN_NEIGHBORHOOD
                .map { coordinate + it }
                .filter { it in map && map[it] == plant }
                .forEach {
                    region.add(it)
                    flood(it, plant, region)
                }
        }

        map.forEach { (coordinate, plant) ->
            if (coordinate in visited) return@forEach

            val region = mutableSetOf(coordinate)
            flood(coordinate, plant, region)
            regions.add(region)
        }

        return regions.map { it.toSet() }.toSet()
    }

    private fun perimeter(region: Set<Coordinate>): Int =
        region.sumOf { center -> Direction.VON_NEUMANN_NEIGHBORHOOD.count { center + it !in region } }

    private fun sides(region: Set<Coordinate>): Int = region.sumOf { center ->
        Direction.VON_NEUMANN_NEIGHBORHOOD.count {
            center + it !in region && (center + it.turnFullRight() !in region || center + it.turnHalfRight() in region)
        }
    }

    fun totalFencePrice(map: Map<Coordinate, Char>, bulkDiscount: Boolean = false): Int = regions(map).sumOf { region ->
        region.size * if (bulkDiscount) sides(region) else perimeter(region)
    }

}

fun main() {
    GardenGroups().run {
        val map = parseInput(readInput())
        println(totalFencePrice(map))
        println(totalFencePrice(map, bulkDiscount = true))
    }
}
