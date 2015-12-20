object Main {

  def main(args: Array[String]) {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toList
    val boxDimensions = lines.map(_.split("x").map(_.toInt)).collect { case Array(x,y,z) => (x,y,z)}
    val totalAmountOfWrappingPaper = getTotalAmountOfWrappingPaper(boxDimensions)
    val totalFeetOfRibbon = getTotalFeetOfRibbon(boxDimensions)
    println(totalAmountOfWrappingPaper)
    println(totalFeetOfRibbon)
  }

  //Part 1
  private def getTotalAmountOfWrappingPaper(boxDimensions: List[Tuple3[Int,Int,Int]]): Int = {

    val f = (tuple: Tuple3[Int,Int,Int]) => {
      val l = tuple._1
      val w = tuple._2
      val h = tuple._3
      val sideOne = l*w
      val sideTwo = w*h
      val sideThree = h*l
      Vector(2*sideOne + 2*sideTwo + 2*sideThree).sum + List(sideOne,sideTwo,sideThree).min
    }
    applyAndSum(boxDimensions, f)
  }


  //Part 2
  private def getTotalFeetOfRibbon(boxDimensions: List[Tuple3[Int,Int,Int]]): Int = {
    val f = (tuple: Tuple3[Int,Int,Int]) => {
      val l = tuple._1
      val w = tuple._2
      val h = tuple._3
      val perimeterOne = 2 * l + 2 * w
      val perimeterTwo = 2 * w + 2 * h
      val perimeterThree = 2 * l + 2 * h
      List(perimeterOne, perimeterTwo, perimeterThree).min + l*w*h
    }
    applyAndSum(boxDimensions, f)
  }

  //Applies the passed function to each element in the list of tuples and then sums up the result
  private def applyAndSum(tuples: List[Tuple3[Int,Int,Int]], f: Tuple3[Int,Int,Int] => Int): Int =
    tuples.map(f(_)).sum

}
