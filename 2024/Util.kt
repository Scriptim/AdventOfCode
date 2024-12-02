fun <T> List<T>.toPair(): Pair<T, T> = this[0] to this[1]

fun <T> List<T>.dropAt(index: Int): List<T> = this.take(index) + this.drop(index + 1)
