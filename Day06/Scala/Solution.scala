import java.awt.Point

object Main {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toList
    val actions = lines.map(Action.getAction)

    //part 1
    val turnLights = (action: Action) => {
      action match {
        case TurnOn(_,_) => num: Int => 1
        case TurnOff(_,_) => num: Int => 0
        case Toggle(_,_) => num: Int => if(num == 1) 0 else 1
      }
    }

    //part 2
    val brightenLights = (action: Action) => {
      action match {
        case TurnOn(_,_) => num: Int => num + 1
        case TurnOff(_,_) => num: Int => if(num > 0) num - 1 else 0
        case Toggle(_,_) => num: Int => num + 2
      }
    }

    //partially apply functions
    val getNumLightsLit = changeLights(turnLights)(_)
    val getTotalBrightness = changeLights(brightenLights)(_)


    println(getNumLightsLit(actions))
    println(getTotalBrightness(actions))
  }

  
  def changeLights(changeCell: Action => Int => Int)(actions: List[Action]): Int = {

    //2-d matrix to mutate :(
    val grid = Array.ofDim[Int](1000, 1000)

    //apply the given function to each cell in the submatrix covered by the start and end points
    def changeLights(start: Point, end: Point, f: Int => Int): Unit = {
      Range(start.x, end.x).inclusive.foreach(x => {
        Range(start.y, end.y).inclusive.foreach(y => {
          grid(x)(y) = f(grid(x)(y))
        })
      })
    }

    actions.foreach(action => {
      //get the function we should apply to each cell for the given action
      val f = changeCell(action)
      action match {
        case TurnOn(start, end) => changeLights(start, end, f)
        case TurnOff(start, end) => changeLights(start, end, f)
        case Toggle(start, end) => changeLights(start, end, f)
      }
    })

    grid.foldRight(0)((row, acc) => acc + row.foldRight(0)(_ + _))
  }
}

//each light can be turned on, off, or toggled
sealed trait Action
case class TurnOn(start: Point, end: Point) extends Action
case class TurnOff(start: Point, end: Point) extends Action
case class Toggle(start: Point, end: Point) extends Action

object Action {

  //parses a line into the appropriate action
  def getAction(line: String): Action = {
    val lineTokens = line.split(" ")
    lineTokens match { 
      case Array(_, "on", first, _, second, _*) => TurnOn(parseCoordinate(first), parseCoordinate(second))
      case Array(_, "off", first, _, second, _*) => TurnOff(parseCoordinate(first), parseCoordinate(second))
      case Array("toggle", first, _, second) => Toggle(parseCoordinate(first), parseCoordinate(second))
    }
  }
  
  def parseCoordinate(coord: String): Point = {
    val coords = coord.split(",")
    coords match {
      case Array(x,y) => new Point(Integer.parseInt(x), Integer.parseInt(y))
      case _ => throw new RuntimeException
    }

  }
}
