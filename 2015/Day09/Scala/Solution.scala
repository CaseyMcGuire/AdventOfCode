


object Main {

  def main(args: Array[String]): Unit = {
    val inputs = scala.io.Source.fromFile(args(0)).getLines.toList
    val graph = Graph.parseLines(inputs)

    //part 1. Get *shortest* distance
    println(getDistance(graph, Integer.MAX_VALUE, Integer.min))

    //part 2. Get *longest* distance
    println(getDistance(graph, Integer.MIN_VALUE, Integer.max))
  }

  //Goes through each path that visits each vertex exactly once. 
  def getDistance(graph: Map[String, Map[String, Int]], initValue: Int, f: (Int, Int) => Int): Int = {
    def recur(curVertex: String, graph: Map[String, Map[String, Int]], runningTotal: Int): Int = {
      if(graph.size == 1) runningTotal
      else {
        val neighbors = graph(curVertex)
        val unvisitedVertices = neighbors.filterKeys(graph.contains(_))

        //visit each neighbor we haven't visited yet
        unvisitedVertices.foldLeft(initValue)((acc: Int, mapping: (String, Int)) =>
          f(recur(mapping._1, graph - curVertex, runningTotal + mapping._2), acc))
      }
    }
    graph.keys.foldLeft(initValue)((acc, x) => f(recur(x, graph, 0), acc))
  }
}


object Graph {

  //parses all lines and returns a map whose keys are each vertice's name and whose value is
  //a map containing its neighbor and the edge's weight
  def parseLines(lines: List[String]): Map[String, Map[String, Int]] = {

    def parseInput(line: String): (String, String, Int) =
      line.split(" ") match {
        case Array(firstCity, _, secondCity, _, distance) => (firstCity, secondCity, Integer.parseInt(distance))
        case _ => throw new RuntimeException
      }

    def recur(lines: List[String], acc: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
      lines match {
        case Nil => acc
        case x::xs => {
          val (first, second, distance) = parseInput(x)
          val firstNeighbors = acc.getOrElse(first, Map[String, Int]()) + (second -> distance)
          val secondNeighbors = acc.getOrElse(second, Map[String, Int]()) + (first -> distance)
          recur(xs, acc + (first -> firstNeighbors, second -> secondNeighbors))
        }
      }
    }


    recur(lines, Map[String, Map[String, Int]]())
  }



}
