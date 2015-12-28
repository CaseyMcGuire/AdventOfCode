import scala.collection._

object Main {
  def main(args: Array[String]): Unit = {
    val inputs = scala.io.Source.fromFile(args(0)).getLines.toList
    val gates: List[Gate] = inputs.map(Gate.parseGate(_))
    //part 1
    println(Circuit.calculateCircuit(gates)("a"))

    //part 2
    val modifiedGates = gates.map {
      case Signal(_, "b") => Signal(16076, "b")
      case n: Any => n
    }
    println(Circuit.calculateCircuit(modifiedGates)("a"))
  }
}


object Circuit {

  /*
   Given a list of gates, returns a map from the gates name to its output value
   */
  def calculateCircuit(gates: List[Gate]): Map[String, Int] = {

    @scala.annotation.tailrec
    def getCircuitValues(acc: Map[String, Int], remainingGates: List[Gate]): Map[String, Int] = {
      remainingGates match {
        case Nil => acc
        case _ => {
         val (definedGates, undefinedGates) = remainingGates.partition(_.getOutput(acc).isDefined)
          val newMap = definedGates.map(gate => gate.getName -> gate.getOutput(acc).get).toMap ++ acc
          getCircuitValues(newMap, undefinedGates)
        }
      }
    }
    val (initialSignals, otherGates) = gates.partition { case Signal(_,_)  => true; case _ => false }
//    getCircuitValues(getNewMap(Map[String, Int](), initialSignals), otherGates)
    val initialMap = initialSignals.map(gate => gate.getName -> gate.getOutput(Map[String, Int]()).get).toMap
    getCircuitValues(initialMap, otherGates)
  }
}

/**
  Every gate has a name and an output value
  */
sealed abstract class Gate(output: String) {

  def getName: String = output
  def getOutput(map: Map[String, Int]): Option[Int] = 
    this match {
      case x: Signal =>       Some(x.value)
      case x: DirectSignal => x.getValue(x.wire, map)
      case x: And =>          x.getValue(x.wire1, x.wire2, map)
      case x: LiteralAnd =>   x.getValue(x.wire, map)
      case x: Or =>           x.getValue(x.wire1, x.wire2, map)
      case x: RightShift =>   x.getValue(x.wire, map)
      case x: LeftShift =>    x.getValue(x.wire, map)
      case x: Not =>          x.getValue(x.wire, map)
      case _ => None
    }
}

/**
  Every gate takes either one or two inputs 
  */
sealed trait OneInputGate {
  def performOp(x: Int): Int

  def getValue(wire: String, map: Map[String,Int]): Option[Int] = 
    map.get(wire) match {
      case Some(x: Int) => Some(performOp(x))
      case _ => None
    }
}

sealed trait TwoInputGate {
  def performOp(x: Int, y: Int): Int

  def getValue(wire1: String, wire2: String, map: Map[String, Int]): Option[Int] =
    (map.get(wire1), map.get(wire2)) match {
      case (Some(x), Some(y)) => Some(performOp(x,y))
      case _ => None
    }
}

/*
 Possible gates
 */
case class Signal(value: Int, output: String) extends Gate(output)
case class DirectSignal(wire: String, output: String) extends Gate(output) with OneInputGate {
  override def performOp(x: Int): Int = x
}

case class LiteralAnd(num: Int, wire: String, output: String) extends Gate(output) with OneInputGate {
  override def performOp(x: Int) = num & x
}
case class And(wire1: String,wire2: String, output: String) extends Gate(output) with TwoInputGate {
  override def performOp(x: Int, y: Int): Int = x & y
}

case class Or(wire1: String, wire2: String, output: String) extends Gate(output) with TwoInputGate {
  override def performOp(x: Int, y: Int): Int = x | y
}

case class RightShift(wire: String, shiftAmount: Int, output: String) extends Gate(output) with OneInputGate {
  override def performOp(x: Int): Int = x >>> shiftAmount
}

case class LeftShift(wire: String, shiftAmount: Int, output: String) extends Gate(output) with OneInputGate {
  override def performOp(x: Int): Int = x << shiftAmount
}
case class Not(wire: String, output: String) extends Gate(output) with OneInputGate {
  override def performOp(x: Int): Int = ~x
}

object Gate {

  //parses a single line of the input file to a gate
  def parseGate(str: String): Gate =
    str.split(" ") match {
      case Array(n, "->", gate) => 
        try { new Signal(Integer.parseInt(n), gate) }
        catch { case e: NumberFormatException => new DirectSignal(n, gate) }
      case Array(wire1, "AND", wire2, _, output) => 
        try { new LiteralAnd(Integer.parseInt(wire1), wire2, output) }
        catch { case e: NumberFormatException => new And(wire1, wire2, output) }
      case Array(wire1, "OR", wire2, _, output) => new Or(wire1, wire2, output)
      case Array(wire, "RSHIFT", num, _, output) => new RightShift(wire, Integer.parseInt(num), output)
      case Array(wire, "LSHIFT", num, _, output) => new LeftShift(wire, Integer.parseInt(num), output)
      case Array("NOT", wire, _, output) => new Not(wire, output)
    }
}

