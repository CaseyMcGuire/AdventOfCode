import scala.util.matching.Regex

object Main {

  private lazy val neededStr: Regex = 
    ('a' to 'z').foldLeft(new StringBuilder("("))((acc, a) => acc.append(a.toString + "{2}|"))
                .dropRight(1)//gets rid of trailing "|"
                .append(")")
                .toString
                .r



  def main(args: Array[String]): Unit = {
    val input = "hxbxwxba"

    val getValidPassword = continuallyApply(increment)(isValidPassword)(_)

    //part 1
    val answer = getValidPassword(input)
    println(answer)

    //part 2
    println(getValidPassword(increment(answer)))
  }

  //continually applies the function f until the function g returns true. 
  def continuallyApply[A](f: A => A)(g: A => Boolean)(start: A): A = {
    @scala.annotation.tailrec
    def recur(elem: A): A = 
      if(g(elem)) elem
      else        recur(f(elem))
    recur(start)
  }

  def isValidPassword(password: String): Boolean = 
    containsOneIncreasingStraight(password) &&
    containsTwoDifferentNonOverlappingPairs(password) &&
    !containsForbiddenChars(password)

  //increments a string by 1
  //ex) "a" -> "b"
  //ex) "az" -> "ba"
  //ex) "zz" -> "aaa"
  def increment(str: String): String = {
    val builder = new StringBuilder(str)

    @scala.annotation.tailrec
    def increment(index: Int): Unit = {
      val curChar = builder.charAt(index)
      if(index < 0) builder.insert(0, 'a')
      else if(curChar != 'z') {
        builder.setCharAt(index, (curChar + 1).toChar)
      }
      else {
        builder.setCharAt(index, 'a')
        increment(index - 1)
      }
    }
    increment(builder.length - 1)
    builder.toString
  }

  //returns true if this string contains at least one increasing straight ("abc", "bcd", "cde", etc)
  //ex) "abcd" -> true ("abc")
  //ex) "abdefhg" -> false
  def containsOneIncreasingStraight(str: String): Boolean = {
    @scala.annotation.tailrec
    def recur(str: List[Char]): Boolean = 
      str match {
        case x::y::z::xs => if(isIncreasingStraight(x,y,z)) true else recur(y::z::xs)
        case _ => false
      }

    def isIncreasingStraight(a: Char, b: Char, c: Char): Boolean = 
      return a == (b-1) && a == (c - 2)

    recur(str.toCharArray.toList)
  }
    

  def containsTwoDifferentNonOverlappingPairs(str: String): Boolean = 
    neededStr.findAllIn(str).toSet.size >= 2

  def containsForbiddenChars(str: String): Boolean = 
    "[i|o|l]".r.findAllIn(str).nonEmpty
}
