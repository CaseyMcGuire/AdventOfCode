//deprecated API :(
import scala.util.parsing.json.JSON._

object Main {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile(args(0)).mkString
    //part 1
    val nums = getAllNumbers(input).sum
    println(nums)

    //part 2
    println(getAllNumbersExceptRed(input).sum)
  }

  //returns all numbers in a string. Contiguous numbers are concantenated into a single number
  //ex)getAllNumbers("-12sadf13 145a32") -> List(-12, 13, 145, 32)
  def getAllNumbers(str: String): List[Int] = 
    "-?\\d+".r.findAllIn(str).toList.map(_.toInt)


  //Returns all numbers except those that are inside an object that contains the value "red"
  def getAllNumbersExceptRed(str: String): List[Double] = {

    def parseObject(obj: Map[String, Any]): List[Double] = {
      if (obj.valuesIterator.contains("red")) 
        List[Double]()
      else
        obj.foldLeft(List[Double]())((acc, pair) => acc ++ parse(pair._2))
    }

    def parseArray(arr: List[Any]): List[Double] = {
      arr.foldLeft(List[Double]())((acc, elem) => acc ++ parse(elem))
    }

    def parse(elem: Any): List[Double] = {
      elem match {
        case lst: List[Any] => parseArray(lst)
        case map: Map[String, Any] => parseObject(map) //compiler gives warning here
        case num: Double => List[Double](num)
        case _ => List[Double]()
      }
    }

    val json = parseFull(str)
    json match {
      case Some(x) => parse(json.get)
      case _ => List[Double]()
    }
  }
}

