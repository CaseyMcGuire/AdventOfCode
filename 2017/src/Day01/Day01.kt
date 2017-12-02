package day01

import java.io.File
import java.util.*


fun main(args: Array<String>) {
    val scanner = Scanner(File("./src/day01/input.txt"))
    //println(File("./").listFiles().forEach(::println))
    val input = scanner.next()

    //part 1
    val total1 = calculateNeighboringPairedNumbers(input)
    println(total1)

    //part 2
    val total2 = calculateHalfwayPairedNumbers(input)
    println(total2)
}

fun calculateNeighboringPairedNumbers(input: String): Int {
    val inputLessOne = input.substring(1, input.length)
    val pairs = input.zip(inputLessOne)
    var total = pairs.fold(0, {total, next -> if (next.first == next.second) total + Character.getNumericValue(next.first) else total })
    if (input.get(0) == input.get(input.length - 1)) {
        total += Character.getNumericValue(input.get(0))
    }
    return total
}

fun calculateHalfwayPairedNumbers(input: String): Int {
    val firstHalf = input.substring(0, input.length / 2)
    val secondHalf = input.substring(input.length / 2, input.length)
    val pairs = firstHalf.zip(secondHalf)
    val total = pairs.fold(0, {total, next -> if (next.first == next.second) total + Character.getNumericValue(next.first) * 2 else total })
    return total
}