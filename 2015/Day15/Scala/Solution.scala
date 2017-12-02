
object Main {

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toList
    val ingredients = lines.map(parseLineToIngredient(_))


    val combos = (0 to 100).combinations(ingredients.size)
                           .filter(_.sum == 100)
                           .flatMap(_.permutations)
                           .map(_.zip(ingredients))
                           .map(reverseMap(_))
                           .toList

    //part 1
    val highestScore = combos.map(getTotalScore(_))
                             .max

    //part 2
    val highestScoreOf500CalorieCookie = combos.filter(getTotalCalories(_) == 500)
                                               .map(getTotalScore(_))
                                               .max

    println(highestScore)
    println(highestScoreOf500CalorieCookie)
  }

  /**
    * Given a sequence of Tuple2's, returns a map where each key-value pair is 
    * the Tuple2's second element mapped to the first.
    * 
    * @param coll The sequence to convert
    */
  def reverseMap[A,B](coll: IndexedSeq[Tuple2[A,B]]): Map[B,A] = {
    coll.foldRight(Map[B,A]())((x,acc) => acc + (x._2 -> x._1))
  }

  /**
    * Given a map whose keys are ingredients and whose corresponding values are 
    * the amount of each ingredient, return the total score of this combination 
    * as laid out in the problem.
    * 
    * @param ingredients The ingredient combination of the this cookie
    * @return The score of this combination of ingredients
    */
  def getTotalScore(ingredients: Map[Ingredient, Int]): Long = {
    val ingredientFields =
      List(
        (ingredient: Ingredient) => ingredient.capacity,
        (ingredient: Ingredient) => ingredient.durability,
        (ingredient: Ingredient) => ingredient.flavor,
        (ingredient: Ingredient) => ingredient.texture
        )

    ingredientFields.foldRight(1L)((curField, acc) => {
      val propertyScore =  getScore(ingredients, curField)
      if(propertyScore < 0) 0
      else                  propertyScore * acc
    })
  }

  /**
    * Returns the score of an ingredient for a particular property.
    * 
    * @param ingredients A map whose keys are the ingredient and whose corresponding value
    *                    is the amount of that ingredient
    * @param f A function to apply to each ingredient in the map. This is intended to pull
    *          out what property we're currently calculating the score for.
    * @return The total score of all the ingredient for the particular property.
    */
  private def getScore(ingredients: Map[Ingredient, Int], f: (Ingredient => Int)): Long =
    ingredients.foldRight(0L)((keyVal, acc) => {
      val (property, ingredientAmount) = keyVal
      f(property) * ingredientAmount + acc
    })

  /**
    * Calculate the total calories of the given ingredients.
    * 
    * @param ingredients A map whose keys are the ingredient and whose corresponding value
    *                     is the amount of that ingredient
    * @return The total number of calories in the given cookie.
    */
  def getTotalCalories(ingredients: Map[Ingredient, Int]): Long = 
    getScore(ingredients, _.calories)


  /**
    * Parses each line of the input file into an ingredient.
    */
  private def parseLineToIngredient(line: String): Ingredient = {
    line.split(" ") match {
      case Array(name, _, capacity, _, durability, _, flavor, _, texture, _, calories) => {
        //have to drop last character for some due to hanging punctuation when splitting on whitespace
        Ingredient(name.dropRight(1),
          Integer.parseInt(capacity.dropRight(1)),
          Integer.parseInt(durability.dropRight(1)),
          Integer.parseInt(flavor.dropRight(1)),
          Integer.parseInt(texture.dropRight(1)),
          Integer.parseInt(calories))
      }
      case _ => throw new RuntimeException
    }
  }
}

case class Ingredient(val name: String, val capacity: Int, val durability: Int, val flavor: Int, val texture: Int, val calories: Int)
