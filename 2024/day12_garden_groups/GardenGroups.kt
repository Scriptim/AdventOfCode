package day12_garden_groups

import Direction
import Vector2D
import parse2DMap
import plus
import java.io.File

class GardenGroups {

    fun readInput(): List<String> = File("src/day12_garden_groups/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Vector2D<Int>, Char> = parse2DMap(lines)

    private fun regions(map: Map<Vector2D<Int>, Char>): Set<Set<Vector2D<Int>>> {
        val visited = mutableSetOf<Vector2D<Int>>()
        val regions = mutableSetOf<MutableSet<Vector2D<Int>>>()

        fun flood(coordinate: Vector2D<Int>, plant: Char, region: MutableSet<Vector2D<Int>>) {
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

    private fun perimeter(region: Set<Vector2D<Int>>): Int =
        region.sumOf { center -> Direction.VON_NEUMANN_NEIGHBORHOOD.count { center + it !in region } }

    private fun sides(region: Set<Vector2D<Int>>): Int = region.sumOf { center ->
        Direction.VON_NEUMANN_NEIGHBORHOOD.count {
            center + it !in region && (center + it.turnFullRight() !in region || center + it.turnHalfRight() in region)
        }
    }

    fun totalFencePrice(map: Map<Vector2D<Int>, Char>, bulkDiscount: Boolean = false): Int = regions(map).sumOf { region ->
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
