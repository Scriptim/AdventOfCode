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

    private fun maximumClique(connections: Set<Pair<String, String>>): Set<String> {
        val adjacencyMap = adjacencyMap(connections)
        val maximalCliques = mutableSetOf<Set<String>>()

        fun bronKerbosch(include: Set<String>, maybe: Set<String>, exclude: Set<String>) {
            if (maybe.isEmpty() && exclude.isEmpty()) maximalCliques.add(include)
            val visited = mutableSetOf<String>()
            maybe.forEach { computer ->
                bronKerbosch(
                    include + computer,
                    maybe.minus(visited).intersect(adjacencyMap[computer]!!),
                    exclude.plus(visited).intersect(adjacencyMap[computer]!!)
                )
                visited.add(computer)
            }
        }

        bronKerbosch(emptySet(), adjacencyMap.keys, emptySet())

        return maximalCliques.maxBy { it.size }
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

    fun password(connections: Set<Pair<String, String>>): String = maximumClique(connections).sorted().joinToString(",")

}

fun main() {
    LANParty().run {
        val connections = parseInput(readInput())
        println(countThreeCliques(connections))
        println(password(connections))
    }
}
