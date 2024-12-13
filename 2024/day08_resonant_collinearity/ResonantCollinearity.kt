package day08_resonant_collinearity

import Vector2D
import minus
import parse2DMap
import plus
import java.io.File

class ResonantCollinearity {

    fun readInput(): List<String> = File("src/day08_resonant_collinearity/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Vector2D<Int>, Char> = parse2DMap(lines)

    fun antinodeLocations(map: Map<Vector2D<Int>, Char>, resonantHarmonics: Boolean = false): Int {
        val locations = mutableSetOf<Vector2D<Int>>()
        val antennaLocations = map.filterValues { it != '.' }.entries.groupBy({ it.value }, { it.key })

        antennaLocations.values.forEach { sameFrequencyAntennas ->
            sameFrequencyAntennas.forEach { antenna1 ->
                sameFrequencyAntennas.filter { it != antenna1 }.forEach { antenna2 ->
                    val delta = antenna2 - antenna1

                    if (resonantHarmonics) {
                        var antinode = antenna1
                        while (antinode in map) { locations.add(antinode); antinode -= delta }
                        antinode = antenna2
                        while (antinode in map) { locations.add(antinode); antinode += delta }
                    } else {
                        if (antenna1 - delta in map) locations.add(antenna1 - delta)
                        if (antenna2 + delta in map) locations.add(antenna2 + delta)
                    }
                }
            }
        }

        return locations.size
    }

}

fun main() {
    ResonantCollinearity().run {
        val map = parseInput(readInput())
        println(antinodeLocations(map))
        println(antinodeLocations(map, resonantHarmonics = true))
    }
}
