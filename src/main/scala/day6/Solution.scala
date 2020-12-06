package day6

import scala.annotation.tailrec
import scala.io.Source.fromFile

object Solution extends App {
  type Question = String
  type Group = Set[Question]

  def parse(lines: List[String]): List[Group] = {
    @tailrec
    def parse(lines: List[String], curr: List[Group], acc: List[Group]): List[Group] = {
      lines.headOption match {
        case None =>
          val union = curr.reduce((left, right) => left.intersect(right))
          union :: acc

        case Some(v) if v.isEmpty => parse(lines.tail, Nil, {
          val union = curr.reduce((left, right) => left.intersect(right))
          union :: acc
        })
        case Some(v) =>
          val answers = v.split("").toSet
          parse(lines.tail, answers :: curr, acc)
      }
    }

    parse(lines, Nil, Nil)
  }

  val source = fromFile(args(0))
  val lines = source.getLines().toList
  source.close()

  val answer = parse(lines)
    .map(_.size)
    .sum

  println(answer)
}
