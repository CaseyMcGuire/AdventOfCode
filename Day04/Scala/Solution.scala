import java.security._

object Main {

  def main(args: Array[String]): Unit = {
    val secretKey = "iwrupvqb"
    //    val secretKey = "abcdef"

    val md5InHexStartsWith: (Int, Int) => Boolean = (num: Int, numZeroes: Int) => {
      val curKey = secretKey + num

      val md5 = MessageDigest.getInstance("MD5").digest(curKey.getBytes)
      val hex = md5.map("%02x".format(_)).mkString

      hex.take(numZeroes).forall(_ == '0')
    }
    val answer = Iterator.from(0).find(md5InHexStartsWith(_, 5))
    val answer2 = Iterator.from(0).find(md5InHexStartsWith(_, 6))
    println("answer " + answer)
    println("answer2 " + answer2)
  }
}
