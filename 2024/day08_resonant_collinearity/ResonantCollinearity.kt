package day08_resonant_collinearity

import Coordinate
import minus
import plus
import java.io.File

class ResonantCollinearity {

    fun readInput(): List<String> = File("src/day08_resonant_collinearity/input.txt").readLines()

    fun parseInput(lines: List<String>): Map<Coordinate, Char> = lines.flatMapIndexed { row, line ->
        line.mapIndexed { column, char -> Pair(column, row) to char }
    }.toMap()

    fun antinodeLocations(map: Map<Coordinate, Char>): Int {
        val locations = mutableSetOf<Coordinate>()
        val antennaLocations = map.filterValues { it != '.' }.entries.groupBy({ it.value }, { it.key })

        antennaLocations.values.forEach { sameFrequencyAntennas ->
            sameFrequencyAntennas.forEach { antenna1 ->
                sameFrequencyAntennas.filter { it != antenna1 }.forEach { antenna2 ->
                    val delta = antenna2 - antenna1

                    if (antenna1 - delta in map) locations.add(antenna1 - delta)
                    if (antenna2 + delta in map) locations.add(antenna2 + delta)
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
    }
}
