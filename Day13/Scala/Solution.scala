
object Main {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toList


    //get people's preferences
    val preferences: Map[String, Map[String, Int]] = 
      lines.foldLeft(Map[String, Map[String, Int]]())((acc, a) => {
        val (name1, name2, liking) = parseLine(a)
        val firstMap = acc.getOrElse(name1, Map[String, Int]())
        val secondMap = firstMap + (name2 -> liking)
        acc + (name1 -> secondMap)
      })

    val maxHappinessWithoutMe = calculateMaxHappiness(preferences)
    println(maxHappinessWithoutMe)

    val preferencesWithMe = addMeToPreferences(preferences)
    val maxHappinessWithMe = calculateMaxHappiness(preferencesWithMe)
    println(maxHappinessWithMe)
  }

  //Add my preferences and add me to everybody else's preferences
 private def addMeToPreferences(preferences: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
    val myName = "Casey"
    val allNames = preferences.keys.toList

    val myMap = (for {
      name <- allNames
    } yield(name -> 0)).toMap


    //add me to everybody else's maps
    def recur(names: List[String], acc: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = {
      names match {
        case Nil => acc
        case x::xs => {
          val curMap = acc(x) + (myName -> 0)
          recur(xs, acc + (x -> curMap))
        }
      }
    }
    recur(allNames, preferences) + (myName -> myMap)
  }

  private def calculateMaxHappiness(preferences: Map[String, Map[String, Int]]): Int = {
    val allSeatingArrangements = preferences.keys.toVector.permutations
    allSeatingArrangements.foldLeft(Integer.MIN_VALUE)((acc, a) => Integer.max(acc, calculateTotalChangeInHappiness(a, preferences)))
  }

  private def calculateTotalChangeInHappiness(table: Vector[String], preferences: Map[String, Map[String, Int]]): Int = {

    @scala.annotation.tailrec
    def recur(index: Int, acc: Int): Int = {
      if(index >= table.length) acc
      else {
        val prevIndex = if(index == 0) table.length - 1 else index - 1
        val nextIndex = if(index == table.length - 1) 0 else index + 1

        //get the names of the people
        val curPerson = table(index)
        val nextPerson = table(prevIndex)
        val prevPerson = table(nextIndex)

        val curPersonPreferences = preferences(curPerson)
        val nextPreference = curPersonPreferences(nextPerson)
        val prevPreference = curPersonPreferences(prevPerson)
        recur(index + 1, acc + nextPreference + prevPreference)
      }
    }
    recur(0, 0)
  }


  private def parseLine(str: String): Tuple3[String, String, Int] =
    //dropRight on name2 gets rid of trailing period
    str.split(" ") match {
      case Array(name1, _, "gain", num, _, _, _, _, _, _, name2) => (name1, name2.dropRight(1), Integer.parseInt(num))
      case Array(name1, _, "lose", num, _, _, _, _, _, _, name2) => (name1, name2.dropRight(1), -Integer.parseInt(num))
      case x => throw new RuntimeException
    }


}
