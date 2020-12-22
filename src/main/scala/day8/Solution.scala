package day8

import scala.annotation.tailrec
import scala.io.Source

case class State[T](value: T) {
  def newState(fn: State[T] => State[T]): State[T] = fn.apply(this)
}

sealed class Op

case class Acc(c: Int) extends Op

case class NoOp(c: Int) extends Op

case class Jmp(c: Int) extends Op


class OperationParser(s: List[String]) {

  def parse(): List[Op] = {
    s.map(_.split(" "))
      .map(parts => (parts(0), parts(1).toInt))
      .map {
        case ("acc", v) => Acc(v)
        case ("nop", v) => NoOp(v)
        case ("jmp", v) => Jmp(v)
      }
  }
}

class Interpreter {
  type Program = List[Op]

  @tailrec
  final def run(program: Program, instruction: Int = 0, state: State[Int] = State(0), visited: Set[Int] = Set()): (Boolean, Int) = {
    if (visited contains instruction) {
      (false, state.value)
    } else {
      val op = program.lift(instruction)
      op match {
        case None => (true, state.value)
        case Some(op) =>
          val v = visited ++ Set(instruction)
          op match {
            case NoOp(_) => run(program, instruction + 1, state, v)
            case Acc(c) => run(program, instruction + 1, state.newState(s => State(s.value + c)), v)
            case Jmp(c) =>  run(program,  instruction + c, state, v)
          }
      }
    }
  }

  def findTerminating(program: Program): Option[Int] = {
    val operatorIndices = program.zipWithIndex.filter {
      case (NoOp(_), _) => true
      case (Jmp(_), _) => true
      case _ => false
    }.map {
      case (_, i) => i
    }

    operatorIndices.map(i => program.splitAt(i))
      .map {
        case (left, right) => right match {
          case x :: xs => x match {
            case NoOp(v) => left ++ (Jmp(v) :: xs)
            case Jmp(v) => left ++ (NoOp(v) :: xs)
          }
        }
      }.map(program => run(program))
      .find(_._1)
      .map(_._2 )
  }
}

object Solution extends App {

  val p = args(0)
  val s = Source.fromFile(p)
  val f = s.getLines().toList
  s.close()

  val ops = new OperationParser(f).parse()

  val stack = new Interpreter
  val res = stack.run(ops)

  println(res)

  val x = stack.findTerminating(ops)
  println(x)
}
