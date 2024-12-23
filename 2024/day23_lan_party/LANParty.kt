package day23_lan_party

import toPair
import java.io.File

class LANParty {

    fun readInput(): List<String> = File("src/day23_lan_party/input.txt").readLines()

    fun parseInput(lines: List<String>): Set<Pair<String, String>> = lines.map { it.split("-").toPair() }.toSet()

    private fun adjacencyMap(connections: Set<Pair<String, String>>): Map<String, Set<String>> {
        val map = mutableMapOf<String, MutableSet<String>>()
        connections.forEach { (a, b) ->
            map.computeIfAbsent(a) { mutableSetOf() }.add(b)
            map.computeIfAbsent(b) { mutableSetOf() }.add(a)
        }
        return map.mapValues { it.value.toSet() }.toMap()
    }

    fun countThreeCliques(connections: Set<Pair<String, String>>): Int {
        val threeCliques = mutableSetOf<Set<String>>()
        val adjacencyMap = adjacencyMap(connections)
        adjacencyMap.keys.filter { it.startsWith('t') }.forEach { computer1 ->
            adjacencyMap[computer1]!!.forEach { computer2 ->
                adjacencyMap[computer2]!!.filter { it in adjacencyMap[computer1]!! }.forEach { computer3 ->
                    threeCliques.add(setOf(computer1, computer2, computer3))
                }
            }
        }
        return threeCliques.size
    }

}

fun main() {
    LANParty().run {
        val connections = parseInput(readInput())
        println(countThreeCliques(connections))
    }
}
