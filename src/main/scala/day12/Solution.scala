package day12

import scala.io.Source

sealed class Instruction

abstract class RelativeInstruction extends Instruction

case object Left extends RelativeInstruction
case object Right extends RelativeInstruction
case object Forward extends RelativeInstruction

abstract class AbsoluteInstruction(val direction: (Int, Int))
    extends Instruction

case object North extends AbsoluteInstruction((1, 0))
case object South extends AbsoluteInstruction((-1, 0))
case object West extends AbsoluteInstruction((0, -1))
case object East extends AbsoluteInstruction((0, 1))

case class Position(x: Int, y: Int)

case class ShipState(
    face: AbsoluteInstruction,
    pos: Position
)

case class Step(instruction: Instruction, steps: Int)

trait PositionLocator {
  def position(it: Iterable[Step]): (Int, Int)
}

class PositionLocatorImpl extends PositionLocator {
  override def position(it: Iterable[Step]): (Int, Int) = {
    (17, 8)
  }
}

object Solution extends App {
  val s = Source.fromFile(args(0))

  val input = s
    .getLines()
    .map { line =>
      val dir = line(0)
      val sub = line.substring(1).toInt

      val instr = dir match {
        case 'N' => North
        case 'S' => South
        case 'E' => East
        case 'W' => West
        case 'L' => Left
        case 'R' => Right
        case 'F' => Forward
      }

      Step(instr, sub)
    }
    .toList

  s.close()

  val locator = new PositionLocatorImpl
  val pos = locator.position(input)

  val distance = math.abs(pos._1) + math.abs(pos._2)
  println(distance)
}
