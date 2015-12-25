import java.awt.Point

object Main {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(args(0)).getLines.mkString("").toList

    //part 1
    println(getHousesWithPresents(input).size)

    //part 2
    val (santaDirections, roboSantaDirections) = input.grouped(2)
      .foldRight((List[Char](), List[Char]()))((a, acc) => {
        (a, acc) match {
          case (List(x,y), (list1, list2)) => (x::list1, y::list2)
        }
      })

    val housesVisitedBySanta = getHousesWithPresents(santaDirections)
    val housesVisitedByRoboSanta = getHousesWithPresents(roboSantaDirections)
    println((housesVisitedBySanta ++ housesVisitedByRoboSanta).size)
  }

  private def getHousesWithPresents(directions: List[Char]): Set[Point] = {
    def getNewHouse(direction: Char, curPlace: Point): Point = 
      direction match {
        case '>' => new Point(curPlace.x + 1, curPlace.y)
        case '<' => new Point(curPlace.x - 1, curPlace.y)
        case '^' => new Point(curPlace.x, curPlace.y + 1)
        case 'v' => new Point(curPlace.x, curPlace.y - 1)
      }

    @scala.annotation.tailrec
    def count(directions: List[Char], curHouse: Point, visitedHouses: Set[Point]): Set[Point] =
      directions match {
        case x::xs => {
          val newHouse = getNewHouse(x, curHouse)
          count(xs, newHouse, visitedHouses + newHouse)
        }
        case Nil => visitedHouses
      }

    count(directions, new Point(0,0), Set(new Point(0,0)))
  }
}
