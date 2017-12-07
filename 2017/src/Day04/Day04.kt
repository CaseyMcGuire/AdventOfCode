package Day04

import java.io.File
import java.util.*

fun main(args: Array<String>): Unit {
  val scanner = Scanner(File("./src/Day04/input.txt"))
  val lines = ArrayList<List<String>>()
  while (scanner.hasNextLine()) {
    lines.add(scanner.nextLine().split(" "))
  }

  //part 1
  println(findNumUniqueLines(lines))

  //part 2
  println(findNumNonAnagramLines(lines))
}

fun findNumUniqueLines(lines: List<List<String>>): Int {
  return lines.filter { line -> line.toHashSet().size == line.size }
              .size
}

fun findNumNonAnagramLines(lines: List<List<String>>): Int {
  val sortedWordsInLine = lines.map { words -> words.map { word ->
    val chars = word.toCharArray()
    Arrays.sort(chars)
    String(chars)
  } }
  return findNumUniqueLines(sortedWordsInLine)
}