
object Main {

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toArray
    val (replacements, inputs) = lines.span(!_.isEmpty)
    val (replacementRules, input) = (parseLines(replacements), inputs(1))

    //part 1
    println(getNumDistinctMolecules(replacementRules, input))

    //part 2
  }

  def getNumDistinctMolecules(replacementRules: Map[String, List[String]], input: String): Int = {
    
    def getDistinctMolecules(passedChars: Array[String], remainingChars: List[String], acc: Set[String]): Set[String] = {
      remainingChars match {          
        case x::y::xs => {
          val newPassedChars = passedChars :+ x
          val a = getMolecules(passedChars, x, y::xs)
          val c = getMolecules(passedChars, x + y, xs)
          getDistinctMolecules(newPassedChars, y::xs, acc ++ a ++ c)
        }
        case x::Nil => {
          val newPassedChars = passedChars :+ x
          val a = getMolecules(passedChars, x, List[String]())
          getDistinctMolecules(newPassedChars, Nil, a ++ acc)
        }
        case Nil => acc
      }
    }

    def getMolecules(passedChars: Array[String], curRule: String, remainingChars: List[String]): Set[String] = {
      val mappings = replacementRules.getOrElse(curRule, List[String]())
      mappings.foldRight(Set[String]())((elem, acc) => acc + (passedChars.mkString + elem.mkString + remainingChars.mkString))
    }

    getDistinctMolecules(Array[String](), input.split("").toList, Set[String]()).size
  }

//  def fewestNumOfStepsToGetMedicineMolecule(replacementRules: Map[String, List[String]], input: String): Int = {

//  }


  private def parseLines(lines: Array[String]): Map[String, List[String]] = {
    lines.foldLeft(Map[String, List[String]]())((acc, line) => {
      line.split(" ") match {
        case Array(input, _, generation) => {
          val existingRules = acc.getOrElse(input, List[String]())
          acc + (input -> (generation :: existingRules))
        }
      }
    })
  }
}
