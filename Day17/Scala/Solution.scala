
object Main {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(args(0)).getLines.toList
    val containers = input.map(parseLine(_))

    //part 1
    val combinations = getCombinations(containers, containers.size)

    //part 2
    val combinationsWithMinNumberOfContainers = 
      (1 to containers.size).foldLeft(0)((acc, num) => {
        if(acc != 0) acc
        else         getCombinations(containers, num)
      })

    println(combinations)
    println(combinationsWithMinNumberOfContainers)
  }


  /**
    * Returns the amount of container combinations that sum to 150 and 
    * have less than the max number of containers.
    * 
    * @param lst The list of containers
    * @param maxContainers The maximum of containers
    */
  def getCombinations(lst: List[Int], maxContainers: Int): Int = {

    def getCombinations(lst: List[Int], numContainers: Int, acc: Int): Int = {
      if(acc == 150)
        1
      else if(acc > 150 || numContainers > maxContainers) 
        0
      else 
        lst match {
          case Nil => 0
          case x::xs => getCombinations(xs, numContainers, acc) + 
                        getCombinations(xs, numContainers + 1, x + acc)
        }
    }
    getCombinations(lst.sorted, 0, 0)
  }

  def parseLine(line: String): Int = {
    Integer.parseInt(line)
  }


}
