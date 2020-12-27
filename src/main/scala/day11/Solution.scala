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
      val directions = List(
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1)
      )

      directions.count(
        nextSeat(pos, _, m) match {
          case Taken => true
          case _     => false
        }
      )
    }

    @tailrec
    def nextSeat(
        pos: (Int, Int),
        direction: (Int, Int),
        m: SeatMatrix
    ): Seat = {
      val (x, y) = pos
      val (vert, hor) = direction

      val newX = x + vert
      val newY = y + hor

      if (newX < 0 || newY < 0) {
        Floor
      } else {
        m.drop(newX).headOption match {
          case None => Floor
          case Some(row) =>
            row.drop(newY).headOption match {
              case Some(v) =>
                v match {
                  case Floor => nextSeat((newX, newY), direction, m)
                  case _     => v
                }
              case None => Floor
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
                case Taken if occupiedCount >= 5 =>
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
