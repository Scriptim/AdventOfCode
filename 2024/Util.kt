fun <T> List<T>.toPair(): Pair<T, T> = this[0] to this[1]

fun <T> List<T>.dropAt(index: Int): List<T> = this.take(index) + this.drop(index + 1)

enum class Direction(val delta: Pair<Int, Int>) {
    NORTH(Pair(0, -1)),
    NORTH_EAST(Pair(1, -1)),
    EAST(Pair(1, 0)),
    SOUTH_EAST(Pair(1, 1)),
    SOUTH(Pair(0, 1)),
    SOUTH_WEST(Pair(-1, 1)),
    WEST(Pair(-1, 0)),
    NORTH_WEST(Pair(-1, -1));

    fun turnHalfRight(): Direction = entries[(ordinal + 1) % 8]

    fun turnFullRight(): Direction = entries[(ordinal + 2) % 8]

    fun turnAround(): Direction = entries[(ordinal + 4) % 8]
}

typealias Coordinate = Pair<Int, Int>

operator fun Coordinate.plus(other: Coordinate): Coordinate =
    Pair(first + other.first, second + other.second)

operator fun Coordinate.minus(other: Coordinate): Coordinate =
    Pair(first - other.first, second - other.second)

operator fun Coordinate.plus(direction: Direction): Coordinate =
    Pair(first + direction.delta.first, second + direction.delta.second)

operator fun Coordinate.minus(direction: Direction): Coordinate =
    Pair(first - direction.delta.first, second - direction.delta.second)
