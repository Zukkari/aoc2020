package day5

import scala.annotation.tailrec
import scala.io.Source.fromFile

case class Seat(row: Int, column: Int)

case class Bounds(min: Int, max: Int)

object Solution extends App {

  def decode(encoded: String, bounds: Bounds)(isUpperBound: Char => Boolean): Int = {
    @tailrec
    def decode(chars: List[Char], bounds: Bounds): Int = {
      chars match {
        case x :: Nil => if (isUpperBound(x)) bounds.max else bounds.min
        case x :: xs =>
          val isUpper = isUpperBound(x)
          val delta = {
            val d = bounds.max - bounds.min
            (d / 2.0).round.toInt
          }

          if (isUpper) {
            decode(xs, Bounds(bounds.min + delta, bounds.max))
          } else {
            decode(xs, Bounds(bounds.min, bounds.max - delta))
          }
      }
    }

    decode(encoded.toCharArray.toList, bounds)
  }

  val rowBound = Bounds(0, 127)
  val columnBound = Bounds(0, 7)

  val source = fromFile(args(0))
  val lines = source.getLines().toList

  source.close()

  val isRowChar: Char => Boolean = c => c == 'B' || c == 'F'

  val seatIds = lines.map { input =>
    val rowCodec = input.takeWhile(isRowChar)
    val columnCodec = input.dropWhile(isRowChar)

    val rowId = decode(rowCodec, rowBound)(_ == 'B')
    val columnId = decode(columnCodec, columnBound)(_ == 'R')

    rowId * 8 + columnId
  }

  val maxId = seatIds.max

  val seat = (0 to maxId)
    .filter(!seatIds.contains(_))
    .filter(id => seatIds.contains(id - 1) && seatIds.contains(id + 1))

  println(seat)
}
