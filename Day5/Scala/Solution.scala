
object Main {

  private val vowels = Set[Char]('a', 'e', 'i', 'o', 'u', 'A', 'E','I','O','U')
  private val forbiddenStrings = Set[String]("ab", "cd", "pq", "xy")

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(args(0)).getLines.toList
    val numNiceStrings = input.count(isNiceString)
    val numBetterNiceStrings = input.count(isBetterNiceString)
    println(numNiceStrings)
    println(numBetterNiceStrings)
  }

  //part 1
  private def isNiceString(str: String): Boolean = {

    def containsThreeVowels(str: List[Char]): Boolean =
      str.filter(vowels.contains(_)).size >= 3

    //tests whether there are two adjacent elements in list that satisfy predicate
    @scala.annotation.tailrec
    def existsTwoChars(str: List[Char], f: (Char, Char) => Boolean): Boolean =
      str match {
        case x::y::xs => if(f(x,y)) true else existsTwoChars(y::xs, f)
        case _::Nil => false
        case Nil => false
      }

    val ar = str.toList
    val characterAppearsTwiceInRow = (x: Char, y: Char) => x == y
    val containsForbiddenStrings = (x: Char, y: Char) => forbiddenStrings.contains(x.toString + y)

    containsThreeVowels(ar) &&
    existsTwoChars(ar, characterAppearsTwiceInRow) &&
    !existsTwoChars(ar, containsForbiddenStrings)
  }

  //part 2
  private def isBetterNiceString(str: String): Boolean = {

    @scala.annotation.tailrec
    def hasRepeatLetterWithOneBetween(str: List[Char]): Boolean = 
      str match {
        case x::y::z::xs => if(x == z) true else hasRepeatLetterWithOneBetween(y::z::xs)
        case _ => false
      }

    def hasNonOverlappingPair(str: List[Char]): Boolean = {

      //returns a map where keys are pairs and values are the number of times
      //that key appears in the string
      @scala.annotation.tailrec
      def getAllPairs(str: List[Char], acc: Map[String, Int]): Map[String, Int] =
        str match {
          case x::y::xs => {
            val curPair = getPair(x,y)
            val numAppearances = acc.getOrElse(curPair, 0) + 1
            getAllPairs(y::xs, acc + (curPair -> numAppearances))
          }
          case _ => acc
        }

      //concantenates passed chars into a string
      def getPair(x: Char, y: Char): String = x.toString + y


      @scala.annotation.tailrec
      def hasNonOverlappingPair(str: List[Char], pairs: Map[String, Int]): Boolean = {
        str match {
          case x::y::z::xs => {
            val curPair = getPair(x,y)
            val nextPair = getPair(y,z)
            val numAppearances = pairs(curPair)

            //If the next element overlaps, there needs to be at least one more pair somewhere else in
            //the string
            val numNeeded = if (curPair == nextPair) 3 else 2

            if(numAppearances >= numNeeded) true
            else hasNonOverlappingPair(y::z::xs, pairs + (curPair -> (numAppearances - 1)))
          }
          case _ => false
        }
      }

      val allPairs = getAllPairs(str, Map[String,Int]())
      hasNonOverlappingPair(str, allPairs)
    }

    val lst = str.toList

    hasRepeatLetterWithOneBetween(lst) &&
    hasNonOverlappingPair(lst)
  }


}
