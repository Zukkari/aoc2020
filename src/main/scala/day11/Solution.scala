package day11

import day11.Solution.SeatMatrix

import scala.annotation.tailrec
import scala.io.Source

sealed class Seat

case object Taken extends Seat
case object Empty extends Seat
case object Floor extends Seat

trait Simulation {
  def simulate(matrix: SeatMatrix): SeatMatrix
}

class FixpointSimulationImpl extends Simulation {
  override def simulate(matrix: SeatMatrix): SeatMatrix = {
    def countOccupied(m: SeatMatrix, pos: (Int, Int)): Int = {
      val (row, seat) = pos
      val positions = List(
        (row - 1, seat - 1),
        (row - 1, seat),
        (row - 1, seat + 1),
        (row, seat - 1),
        (row, seat + 1),
        (row + 1, seat - 1),
        (row + 1, seat),
        (row + 1, seat + 1)
      )

      positions.count {
        case (x, y) =>
          m.drop(x).headOption match {
            case _ if x < 0 => false
            case None       => false
            case Some(r) =>
              r.drop(y).headOption match {
                case _ if y < 0 => false
                case Some(s) =>
                  s match {
                    case Taken => true
                    case _     => false
                  }
                case None => false
              }
          }
      }
    }

    @tailrec
    def fixpoint(prev: SeatMatrix): SeatMatrix = {
      val nextIteration = prev.zipWithIndex.map {
        case (row, rowIndex) =>
          row.zipWithIndex.map {
            case (seat, seatIndex) =>
              val occupiedCount = countOccupied(prev, (rowIndex, seatIndex))
              seat match {
                case Taken if occupiedCount >= 4 =>
                  Empty
                case Empty if occupiedCount == 0 =>
                  Taken
                case x => x
              }
          }
      }

      if (nextIteration == prev) {
        nextIteration
      } else {
        fixpoint(nextIteration)
      }
    }

    fixpoint(matrix)
  }
}

object Solution extends App {
  type SeatMatrix = List[List[Seat]]

  def readMatrix(): SeatMatrix = {
    val s = Source.fromFile(args(0))
    val matrix = s
      .getLines()
      .map(line =>
        line
          .split("")
          .map {
            case "L" => Empty
            case "." => Floor
            case "#" => Taken
          }
          .toList
      )
      .toList

    s.close()
    matrix
  }

  val matrix = readMatrix()
  val sim = new FixpointSimulationImpl

  val simulated = sim.simulate(matrix)

  val occupied = simulated.map(_.count(_ == Taken)).sum
  println(occupied)
}
