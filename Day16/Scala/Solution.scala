
object Main {

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toList
    val aunts = lines.map(parseLine(_))

    //part 1
    println(getIdOfCorrectAuntSue(aunts, _ == _, _ == _))

    //part 2
    println(getIdOfCorrectAuntSue(aunts, _ >= _, _ <= _))
  }

  /**
    * Gets the ID of the proper Aunt Sue.
    * 
    * @param auntSues The list of possible AuntSues
    * @param f The function to apply to tree and cats comparisons
    * @param g The function to apply to pomeranian and goldfish comparisons
    * @return All AuntSues that match the appropriate compounds.
    */
  def getIdOfCorrectAuntSue(auntSues: List[AuntSue], f: (Int, Int) => Boolean, g: (Int, Int) => Boolean): List[AuntSue] = {

    def containsDog(curSue: AuntSue, dog: Dog): Boolean = {
      val foundDog = curSue.dogs.find(curDog => {
        (curDog, dog) match {
          case (Some(Samoyed(_)), Samoyed(_)) => true
          case (Some(Akita(_)), Akita(_)) => true
          case (Some(Pomeranian(_)), Pomeranian(_)) => true
          case (Some(Vizsla(_)), Vizsla(_)) => true
          case _ => false
        }
      })

      if(foundDog.isEmpty) true
      else g(foundDog.get.get.num, dog.num)
    }

    auntSues.filter(_.children.getOrElse(3) == 3)
            .filter(sue => f(sue.cats.getOrElse(7), 7))
            .filter(containsDog(_, Samoyed(2)))
            .filter(containsDog(_, Akita(0)))
            .filter(containsDog(_, Pomeranian(3)))
            .filter(containsDog(_, Vizsla(0)))
            .filter(sue => g(sue.goldfish.getOrElse(5), 5))
            .filter(sue => f(sue.trees.getOrElse(3), 3))
            .filter(_.cars.getOrElse(2) == 2)
            .filter(_.perfumes.getOrElse(1) == 1)
  }

  /**
    * Parses a line from the input file into an AuntSue.
    * 
    * @return The properly parsed AuntSue
    */
  private def parseLine(line: String): AuntSue = {

    def getCompound(ar: Array[String])(compound: String): Option[Int] = {
      val foundCompound = ar.find(_.startsWith(compound))
      if(foundCompound.isEmpty) None
      else Some(Integer.parseInt(foundCompound.get.filter(Character.isDigit)))
   }

    def getDogBreed(dogBreedName: String, num: Int): Dog =
      dogBreedName match {
        case "samoyeds" => Samoyed(num)
        case "pomeranians" => Pomeranian(num)
        case "akitas" => Akita(num)
        case "vizslas" => Vizsla(num)
      }


    val (auntSueId, rest) = line.span(_ != ':')
    val compounds = rest.stripPrefix(":")
                        .split(",")
                        .map(_.trim)
                        .map(_.stripSuffix(","))

    val extractCompound = getCompound(compounds)(_)

    val id       = Integer.parseInt(auntSueId.filter(Character.isDigit))
    val children = extractCompound("children")
    val cats     = extractCompound("cats")


    val dogBreeds = List("samoyeds", "pomeranians", "akitas", "vizslas")

    val dogs     = dogBreeds.map(extractCompound(_))
                            .zip(dogBreeds)
                            .map(dog => {
                              dog match {
                                case (Some(num), breedName) => Some(getDogBreed(breedName, num))
                                case _ => None
                              }
                            })

    val goldfish = extractCompound("goldfish")
    val trees    = extractCompound("trees")
    val cars     = extractCompound("cars")
    val perfumes = extractCompound("perfumes")

    AuntSue(id, children, cats, dogs, goldfish, trees, cars, perfumes)
  }


}

case class AuntSue(val id: Int,
                   val children: Option[Int],
                   val cats: Option[Int], 
                   val dogs: List[Option[Dog]], 
                   val goldfish: Option[Int], 
                   val trees: Option[Int], 
                   val cars: Option[Int], 
                   val perfumes: Option[Int])

sealed abstract class Dog {
  val num: Int
}
case class Samoyed(num: Int) extends Dog
case class Pomeranian(num: Int) extends Dog
case class Akita(num: Int) extends Dog
case class Vizsla(num: Int) extends Dog


