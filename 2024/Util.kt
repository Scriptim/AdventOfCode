fun <T> List<T>.toPair(): Pair<T, T> = this[0] to this[1]

fun <T> List<T>.dropAt(index: Int): List<T> = this.take(index) + this.drop(index + 1)

fun Char.toDigit(): Int = this - '0'

enum class Direction(val delta: Vector2D<Int>) {
    NORTH(Vector2D(0, -1)),
    NORTH_EAST(Vector2D(1, -1)),
    EAST(Vector2D(1, 0)),
    SOUTH_EAST(Vector2D(1, 1)),
    SOUTH(Vector2D(0, 1)),
    SOUTH_WEST(Vector2D(-1, 1)),
    WEST(Vector2D(-1, 0)),
    NORTH_WEST(Vector2D(-1, -1));

    companion object {
        val VON_NEUMANN_NEIGHBORHOOD = listOf(NORTH, EAST, SOUTH, WEST)
    }

    fun turnHalfRight(): Direction = entries[(ordinal + 1) % 8]

    fun turnFullRight(): Direction = entries[(ordinal + 2) % 8]

    fun turnAround(): Direction = entries[(ordinal + 4) % 8]
}

operator fun Direction.times(scalar: Int): Vector2D<Int> = delta * scalar

data class Vector2D<T>(val x: T, val y: T) {

    inline fun <R> map(transform: (T) -> R): Vector2D<R> = Vector2D(transform(x), transform(y))

}

@JvmName("plusInt")
operator fun Vector2D<Int>.plus(other: Vector2D<Int>): Vector2D<Int> = Vector2D(x + other.x, y + other.y)
@JvmName("plusLong")
operator fun Vector2D<Long>.plus(other: Vector2D<Long>): Vector2D<Long> = Vector2D(x + other.x, y + other.y)
@JvmName("plusULong")
operator fun Vector2D<ULong>.plus(other: Vector2D<ULong>): Vector2D<ULong> = Vector2D(x + other.x, y + other.y)

operator fun Vector2D<Int>.plus(direction: Direction): Vector2D<Int> = this + direction.delta

@JvmName("minusInt")
operator fun Vector2D<Int>.minus(other: Vector2D<Int>): Vector2D<Int> = Vector2D(x - other.x, y - other.y)
@JvmName("minusLong")
operator fun Vector2D<Long>.minus(other: Vector2D<Long>): Vector2D<Long> = Vector2D(x - other.x, y - other.y)
@JvmName("minusULong")
operator fun Vector2D<ULong>.minus(other: Vector2D<ULong>): Vector2D<ULong> = Vector2D(x - other.x, y - other.y)

operator fun Vector2D<Int>.minus(direction: Direction): Vector2D<Int> = this - direction.delta

@JvmName("timesInt")
operator fun Vector2D<Int>.times(scalar: Int): Vector2D<Int> = Vector2D(x * scalar, y * scalar)
@JvmName("timesLong")
operator fun Vector2D<Long>.times(scalar: Long): Vector2D<Long> = Vector2D(x * scalar, y * scalar)
@JvmName("timesULong")
operator fun Vector2D<ULong>.times(scalar: ULong): Vector2D<ULong> = Vector2D(x * scalar, y * scalar)

@JvmName("divInt")
operator fun Vector2D<Int>.div(scalar: Int): Vector2D<Int> = Vector2D(x / scalar, y / scalar)
@JvmName("divLong")
operator fun Vector2D<Long>.div(scalar: Long): Vector2D<Long> = Vector2D(x / scalar, y / scalar)
@JvmName("divULong")
operator fun Vector2D<ULong>.div(scalar: ULong): Vector2D<ULong> = Vector2D(x / scalar, y / scalar)

fun parse2DMap(lines: List<String>): Map<Vector2D<Int>, Char> = lines.flatMapIndexed { row, line ->
    line.mapIndexed { column, char -> Vector2D(column, row) to char }
}.toMap()

fun MutableMap<Vector2D<Int>, Char>.replaceStraightLine(start: Vector2D<Int>, line: String, direction: Direction) {
    line.forEachIndexed { index, char ->
        this[start + direction * index] = char
    }
}
