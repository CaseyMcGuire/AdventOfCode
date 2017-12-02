
object Main {
  def main(args: Array[String]) {
    val input = scala.io.StdIn.readLine
    println(getFloor(input))
    println(getBasementPosition(input))
  }

  private def getParenVal(x: Char): Int = 
    x match {
      case '(' =>  1
      case ')' => -1
    }

  def getFloor(line: String): Int = {
    line.toCharArray.foldRight(0)((elem, acc) => acc + getParenVal(elem))
  }

  def getBasementPosition(line: String): Int = {
    line.toCharArray.scanLeft(0)(_ + getParenVal(_)).indexOf(-1)
  }
}
