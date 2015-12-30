object Main {
  def main(args: Array[String]): Unit = {
    val inputs = scala.io.Source.fromFile(args(0)).getLines.toList
    //part 1
    val numCharactersInMemory = inputs.foldLeft(0)((acc, x) => acc + getNumCharacters(x, _ + 0, _ + 1, _ + 1, _ + 1))
    val numLiteralCharacters = inputs.foldLeft(0)((acc, x) => acc + x.length)
    println(numLiteralCharacters - numCharactersInMemory)

    //part 2
    val numCharactersInNewString = inputs.foldLeft(0)((acc, x) => acc + getNumCharacters(x, _ + 2, _ + 3, _ + 3, _ + 2))
    println(numCharactersInNewString - numCharactersInMemory)
  }

  /**
    Returns the number of characters in the string.
    */
  def getNumCharacters(str: String, singleQuote: Int => Int, doubleSlashes: Int => Int, quoteSlash: Int => Int, singleCharAscii: Int => Int):Int = {
    @scala.annotation.tailrec
    def recur(list: List[Char], acc: Int): Int = 
      list match {
        case Nil => acc 
        case '\"'::xs => recur(xs, singleQuote(acc))
        case '\\'::'\\'::xs => recur(xs, doubleSlashes(acc))
        case '\\'::'\"'::xs => recur(xs, quoteSlash(acc))
        case '\\'::'x'::_::_::xs => recur(xs, singleCharAscii(acc))
        case _::xs => recur(xs, 1 + acc)
      }
    recur(str.toCharArray.toList, 0)
  }
}
