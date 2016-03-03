object Main {

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toArray
    val lights = lines.map(_.toCharArray)

    println(getNumLightsOnAfterNumSteps(lights, 100))
  }

  def getNumLightsOnAfterNumSteps(lights: Array[Array[Char]], steps: Int): Int = {
    @scala.annotation.tailrec
    def getNumLightsOnAfterNumSteps(curStep: Int, lights: Array[Array[Char]]): Int = {
      if(curStep == steps) getNumLightsOn(lights)
      else {
        val nextConfiguration = getNextConfiguration(lights)
        getNumLightsOnAfterNumSteps(curStep + 1, lights)
      }
    }

    def getNumLightsOn(lights: Array[Array[Char]]): Int = {
      var sum = 0
      for(i <- 0 until lights.size) {
        for(j <- 0 until lights(i).size) {
          if(lights(i)(j) == '#') sum += 1
        }
      }
      sum
    }

    def getNextConfiguration(lights: Array[Array[Char]]): Array[Array[Char]] = {
      val newLights = Array.ofDim[Char](lights.size, lights(0).size)
      for(i <- 0 until lights.size) {
        for(j <- 0 until lights(0).size) {
          newLights(i)(j) = getNextLight(i, j, lights)
        }

      }

      def getNextLight(x: Int, y: Int, lights: Array[Array[Char]]): Char = {
        var sum = 0
        for(i <- -1 to 1) {
          for(j <- -1 to 1) {
            val numLightsAtSpot = 
            if(i == 0 && j == 0) {
                0
              } else {
                (i + x, j + y) match {
                  case (-1, _) => 0
                  case (_, -1) => 0
                  case (100, _) => 0
                  case (_, 100) => 0
                  case (newX, newY) => if(lights(newX)(newY) == '#') 1 else 0
                }
              }
            sum += numLightsAtSpot
          }
        }
        val isLightOn = lights(x)(y) == '#'
        if(isLightOn) {
          if(sum == 2 || sum == 3)  '#'
          else                      '.'
        } else {
          if(sum == 3) '#'
          else         '.'
        }
      }

      newLights
    }
    getNumLightsOnAfterNumSteps(0, lights)
  }
}
