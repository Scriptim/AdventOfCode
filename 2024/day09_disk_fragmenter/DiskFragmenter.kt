package day09_disk_fragmenter

import java.io.File
import kotlin.math.min

class DiskFragmenter {

    fun readInput(): String = File("src/day09_disk_fragmenter/input.txt").readText()

    fun parseInput(line: String): List<Int> = line.trim().map { it.toString().toInt() }

    private data class Block(val fileId: Int, val size: Int)

    private fun compact(diskMap: List<Int>): List<IndexedValue<Block>> {
        val compactDisk = mutableListOf<IndexedValue<Block>>()

        var position = 0
        diskMap.forEachIndexed { index, size ->
            if (index % 2 == 0) compactDisk.add(IndexedValue(position, Block(index / 2, size)))
            position += size
        }

        var blockBeforeGap = 0
        while (true) {
            while (blockBeforeGap < compactDisk.size - 1 && compactDisk[blockBeforeGap].index + compactDisk[blockBeforeGap].value.size == compactDisk[blockBeforeGap + 1].index) {
                blockBeforeGap++
            }

            if (blockBeforeGap == compactDisk.size - 1) break

            val gapStart = compactDisk[blockBeforeGap].index + compactDisk[blockBeforeGap].value.size
            val gapSize = compactDisk[blockBeforeGap + 1].index - gapStart

            val fillBlock = compactDisk.removeLast()
            val fillId = fillBlock.value.fileId
            val fillSize = min(gapSize, fillBlock.value.size)

            compactDisk.add(blockBeforeGap + 1, IndexedValue(gapStart, Block(fillId, fillSize)))
            if (fillSize < fillBlock.value.size) {
                compactDisk.add(IndexedValue(fillBlock.index, Block(fillId, fillBlock.value.size - fillSize)))
            }
        }

        return compactDisk.toList()
    }

    private fun checksum(compactDisk: List<IndexedValue<Block>>): Long {
        var sum = 0L
        compactDisk.forEach { (startPosition, block) ->
            for (position in startPosition..<startPosition + block.size) sum += position * block.fileId
        }
        return sum
    }

    fun compactChecksum(diskMap: List<Int>): Long = checksum(compact(diskMap))

}

fun main() {
    DiskFragmenter().run {
        val diskMap = parseInput(readInput())
        println(compactChecksum(diskMap))
    }
}
