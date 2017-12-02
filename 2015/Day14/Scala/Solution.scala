
object Main {

  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args(0)).getLines.toList
    val reindeer = lines.map(parseLine(_))
    val longestDistance = reindeer.foldLeft(0)((acc, a) => Integer.max(acc, getDistanceTraveled(a, 2504)))
    val longestDistanceWithBonus = getPointsOfWinningReindeer(reindeer, 2503)
    println(longestDistance)
    println(longestDistanceWithBonus)
  }


  //part 1
  private def getDistanceTraveled(reindeer: Reindeer, numSeconds: Int): Int = {

    @scala.annotation.tailrec
    def loop(passedSeconds: Int, totalDistanceTraveled: Int, isResting: Boolean): Int = {
      val timeRemaining = numSeconds - passedSeconds
      val curTimeInterval = if(isResting) reindeer.restTime else Integer.min(timeRemaining, reindeer.speedDuration)
      val curIntervalDistanceTraveled = if (isResting) 0 else curTimeInterval * reindeer.speed

      if(timeRemaining <= 0) totalDistanceTraveled
      else                   loop(passedSeconds + curTimeInterval, curIntervalDistanceTraveled + totalDistanceTraveled, !isResting)
    }
    loop(0, 0, false)
  }


  private def getPointsOfWinningReindeer(reindeer: List[Reindeer], numSeconds: Int): Int = {

    def loop(curSecond: Int, reindeerToStats: Map[Reindeer, (Boolean, Int, Int,Int)]): Int = {

      //move each reindeer along its path
      val newStats = reindeerToStats.map((mapping: Tuple2[Reindeer, Tuple4[Boolean, Int, Int, Int]]) => {
        val (isCurrentlyResting, timeInCurrentState, totalScore, pointsAccumulated) = mapping._2
        val curReindeer = mapping._1

        val (isStillResting, newTimeInCurrentState) = 
          if(isCurrentlyResting && timeInCurrentState == curReindeer.restTime - 1) (false, 0)
          else if(!isCurrentlyResting && timeInCurrentState == curReindeer.speedDuration - 1) (true, 0)
          else (isCurrentlyResting, timeInCurrentState + 1)

        val distanceTraveledLastSecond = if(!isCurrentlyResting) curReindeer.speed else 0

       (curReindeer, (isStillResting, newTimeInCurrentState, totalScore + distanceTraveledLastSecond, pointsAccumulated))
      })

      //get all reindeer who have the highest score
      val reindeerInLead = newStats.foldLeft((List[Reindeer](),0))((acc, a) => {

        val curReindeerScore = a._2._3
        val curReindeer = a._1
        val (curList, curHighScore) = acc

        if(curList.isEmpty || curHighScore == curReindeerScore) (curReindeer::curList, curReindeerScore)
        else if(curHighScore < curReindeerScore) (List(curReindeer), curReindeerScore)
        else acc
      })

      val leadingReindeer = reindeerInLead._1
      val highScore = reindeerInLead._2

      //adjust points for whose in the lead
      val newStatsAdjustedForLead = newStats.map((mapping: Tuple2[Reindeer, Tuple4[Boolean, Int, Int, Int]]) => {
        val curReindeer = mapping._1
        val (isResting, timeInCurrentState, totalScore, pointsAccumulated) = mapping._2
        if(leadingReindeer.contains(curReindeer)) (curReindeer, (isResting, timeInCurrentState, totalScore, pointsAccumulated + 1))
        else mapping
      })

      lazy val highestPoints = newStatsAdjustedForLead.foldLeft(0)((acc, a) => {
        val curPoints = a._2._4
        if(curPoints > acc) curPoints
        else acc
      })
      if(curSecond < numSeconds) loop(curSecond + 1, newStatsAdjustedForLead)
      else highestPoints
    }

    val startMap = (for {
      curReindeer <- reindeer
    } yield (curReindeer -> (false, 0, 0, 0))).toMap

    loop(0, startMap)
  }

  //parses a single line of the input file into a reindeer
  private def parseLine(line: String): Reindeer = 
    line.split(" ") match {
      case Array(name, _, _, speed, _, _, duration, _, _, _, _, _, _, restDuration, _*) => 
        new Reindeer(name, Integer.parseInt(speed), Integer.parseInt(duration), Integer.parseInt(restDuration))
      case _ => throw new RuntimeException("parse error: " + line)
    }

}

case class Reindeer(val name: String, val speed: Int, val speedDuration: Int, val restTime: Int)
