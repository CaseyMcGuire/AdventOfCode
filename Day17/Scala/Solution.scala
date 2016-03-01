
object Main {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(args(0)).getLines.toList
    val containers = input.map(parseLine(_))

    val combinations = containers.permutations.map(elem => {

      val sum = elem.foldLeft(0)((acc, curContainer) => {
        if(acc == 150) acc
        else           acc + curContainer.size
      })
      sum == 150
    }).filter(_ == true)
      .size

    println(combinations)
  }

  def parseLine(line: String): Container = {
    Container(Integer.parseInt(line))
  }

  case class Container(size: Int)

}
