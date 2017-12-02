object Main {

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toArray
    val lights = lines.map(_.toCharArray)

    //part 1 (need to use input.txt)
    println(getNumLightsOnAfterNumSteps(lights, 100, false))

    //part 2 (need to use input2.txt)
    println(getNumLightsOnAfterNumSteps(lights, 100, true))
  }

  /**
    * Returns the number of lights that will be on after the given number of steps.
    * 
    * @param lights The light grid
    * @param steps The number of steps to perform
    * @param cornerLightsAlwaysOn Whether the corner lights should always be considered on.
    */
  def getNumLightsOnAfterNumSteps(lights: Array[Array[Char]], steps: Int, cornerLightsAlwaysOn: Boolean): Int = {
    @scala.annotation.tailrec
    def getNumLightsOnAfterNumSteps(curStep: Int, lights: Array[Array[Char]]): Int = {
      if(curStep == steps) getNumLightsOn(lights)
      else {
        val nextConfiguration = getNextConfiguration(lights)
        getNumLightsOnAfterNumSteps(curStep + 1, nextConfiguration)
      }
    }

    /**
      * Returns the number of lights that are on in the passed light grid.
      */
    def getNumLightsOn(lights: Array[Array[Char]]): Int = {
      lights.map(_.foldRight(0)((elem, acc) => {
        elem match {
          case '#' => acc + 1
          case _ => acc
        }
      })).sum
    }

    /**
      * Returns the next light grid given the state of the current light grid.
      */
    def getNextConfiguration(lights: Array[Array[Char]]): Array[Array[Char]] = {
      val newLights = Array.ofDim[Char](lights.size, lights(0).size)
       (0 until lights.size).foreach(i => {
         (0 until lights(i).size).foreach(j => {
          newLights(i)(j) = getNextLight(i, j, lights)
        })
      })

      /**
        * Returns the next state of the light at the passed coordinate.
        * 
        * @param x The x-coordinate in the grid
        * @param y The y-coordinate in the grid
        * @param lights The current light grid
        * @return '#' if the light will be on; '.' if the light will be off
        */
      def getNextLight(x: Int, y: Int, lights: Array[Array[Char]]): Char = {
        var numLitNeighbors = 0

        for(i <- -1 to 1) {
          for(j <- -1 to 1) {
            val numLightsAtSpot =
              if(i == 0 && j == 0) {
                0
              } else {
                (i + x, j + y) match {
                  //ignore any case where we go off the grid
                  case (-1, _) => 0
                  case (_, -1) => 0
                  case (100, _) => 0
                  case (_, 100) => 0
                  case (newX, newY) => if(lights(newX)(newY) == '#') 1 else 0
                }
              }
            numLitNeighbors += numLightsAtSpot
          }
        }

        val isLightOn = lights(x)(y) == '#'

        /** @return True if the passed coordinates represent a corner in the board */
        val isCornerLight: (Int, Int) => Boolean = (i, j) => {
          (i, j) match  {
            case (0, 0) => true
            case (0, 99) => true
            case (99, 0) => true
            case (99, 99) => true
            case _ => false
          }
        }

        if(cornerLightsAlwaysOn && isCornerLight(x, y)) {
          '#'
        } 
        else if(isLightOn) {
          if(numLitNeighbors == 2 || numLitNeighbors == 3) '#'
          else                                             '.'
        } else {
          if(numLitNeighbors == 3) '#'
          else                     '.'
        }
      }
      newLights
    }
    getNumLightsOnAfterNumSteps(0, lights)
  }
}
