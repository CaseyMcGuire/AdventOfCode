package Day05

import java.io.File
import java.util.*

fun main(args: Array<String>) {
  val scanner = Scanner(File("./src/Day05/input.txt"))
  val instructions = ArrayList<Int>()
  while(scanner.hasNextLine()) {
    instructions.add(Integer.parseInt(scanner.nextLine()))
  }
  //part 1
  println(findNumJumpsUntilExit(instructions, { instruction -> instruction + 1}))

  //part 2
  println(findNumJumpsUntilExit(instructions, { instruction -> if (instruction >= 3) instruction - 1 else instruction + 1}))
}

fun findNumJumpsUntilExit(instructions: List<Int>, jumpModification: (Int) -> Int): Int {
  val copy = instructions.toMutableList()
  var numJumps = 0
  var index = 0
  while( index >= 0 && index < instructions.size) {
    val instruction = copy[index]
    copy[index] = jumpModification(copy[index])
    index += instruction
    numJumps++
  }
  return numJumps
}