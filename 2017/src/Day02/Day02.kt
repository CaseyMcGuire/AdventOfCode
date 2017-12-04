package Day02

import java.io.File
import java.util.*

fun main(args: Array<String>): Unit {
  val scanner = Scanner(File("./src/Day02/input.txt"))
  val lines = ArrayList<String>()
  while (scanner.hasNextLine()) {
    lines.add(scanner.nextLine())
  }
  //part 1
  val rows = lines.map { line -> line.split("\t").map { num -> Integer.parseInt(num) } }
  println(calculateChecksum(rows))

  println(calculateChecksum2(rows))
}

fun calculateChecksum(rows: List<List<Int>>): Int {
  val largestInEachRow = rows.map{ row -> row.reduce { cur, acc -> if (cur > acc) cur else acc } }
  val smallestInEachRow = rows.map { row -> row.reduce { cur, acc -> if (cur < acc) cur else acc } }
  return largestInEachRow.zip(smallestInEachRow)
                         .map { (largest, smallest) -> largest - smallest}
                         .reduceRight { cur, acc -> cur + acc }
}

fun calculateChecksum2(rows: List<List<Int>>): Int {
  return rows.map(::getOnlyDivisiblePair)
             .map { pair -> pair!!.first / pair!!.second }
             .reduceRight { cur, acc -> cur + acc }
}

//returns the only divisible pair for the given index
private fun getOnlyDivisiblePair(row: List<Int>): Pair<Int, Int>? {
  return row.mapIndexed { index, _ -> getPairsForIndex(index, row) }
            .flatMap { list -> list }
            .find { elem -> elem.first % elem.second == 0 }

}

private fun getPairsForIndex(index: Int, row: List<Int>): List<Pair<Int, Int>> {
  return row.filterIndexed { i, _ -> i != index }
            .map { elem -> Pair(row[index], elem) }
}