
object Main {

  def main(args: Array[String]): Unit = {
    val input = "1113222113"

    //part 1
    val answer = getLookAndSay(input, 40)
    println(answer.size)

    //part 2
    println(getLookAndSay(input, 50).size)
  }

  def getLookAndSay(startVal: String, numTimes: Int): String = 
    (0 until numTimes).foldLeft(startVal)((acc, _) => lookAndSay(acc))

  def lookAndSay(str: String): String = {
    def getSequencePortion(runningChar: Char, curChar: Char, numTimes: Int): String = 
      if(runningChar == curChar ) ""
      else numTimes.toString + runningChar

    @scala.annotation.tailrec
    def recur(str: List[Char], curChar: Char, numTimes: Int, acc: StringBuilder): String = {
      str match {
        case Nil => {
          acc.append(numTimes.toString + curChar)
          acc.toString
        }
        case x::xs => {
          val seqPortion = getSequencePortion(curChar, x, numTimes)
          acc.append(seqPortion)
          if(seqPortion.length == 0) recur(xs, curChar, numTimes + 1, acc)
          else                       recur(xs, x, 1, acc)
        }
      }
    }
    val strLst = str.toList
    recur(strLst.tail, strLst.head, 1, new StringBuilder)
  }
}
