package day3

import scala.annotation.tailrec
import scala.io.Source

case class Slope(x: Int, y: Int)

object Solution {
  type Tile = List[Char]
  type Map = List[Tile]

  private def findPos(pos: Int, max: Int, x: Int): Int = pos + x match {
    case newPos if newPos < max => newPos
    case x =>
      x % max
  }

  private def countTrees(map: Map, slope: Slope, trees: Int = 0, pos: Int = 0): Int = {
    @tailrec
    def count(map: Map, pos: Int, trees: Int, xSlope: Int): Int = {
      map.headOption match {
        case None => trees
        case Some(tile) => {
          val isTree = tile(pos) match {
            case '#' => 1
            case _ => 0
          }

          count(map.tail, findPos(pos, tile.length, xSlope), trees + isTree, xSlope)
        }
      }
    }

    val tiles = map.indices
      .zip(map)
      .filter {
        case (i, _) => i % slope.y == 0
      }.map(_._2)

    count(tiles.toList, 0, 0, slope.x)
  }


  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(args(0))

    val map: Map = source.getLines()
      .map(_.toCharArray.toList)
      .toList

    source.close()

    val slopes = List(
      Slope(1, 1),
      Slope(3, 1),
      Slope(5, 1),
      Slope(7, 1),
      Slope(1, 2)
    )

    val answer = slopes.map(countTrees(map, _))
      .map(BigDecimal(_))
      .product

    println(answer)
  }
}
