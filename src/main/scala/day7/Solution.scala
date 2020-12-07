package day7

import scala.annotation.tailrec
import scala.io.Source.fromFile

sealed class Bag(val color: String)

case class Container(override val color: String, contains: List[Bag]) extends Bag(color)

case class Empty(override val color: String) extends Bag(color)

case class BagRef(override val color: String, count: Int) extends Bag(color)

class BagParser {
  def parseColored(entry: String): Bag = {
    val refs = entry.split(" ", 2)
    BagRef(refs(1).split(" ").dropRight(1).mkString(" "), refs(0).toInt)
  }

  def parse(line: String): Bag = {
    val parts = line.split(" bags contain ")
    parts(1) match {
      case "no other bags." => Empty(parts(0))
      case others =>
        val otherBags = others.split(", ").map(parseColored).toList
        Container(parts(0), otherBags)
    }
  }
}

trait Counter {
  def count(color: String, bags: List[Bag]): Int
}

object Solution extends App {
  val containsCounter = new Counter {
    def direct(color: String, bags: List[Bag]): List[String] = {
      bags.filter {
        case Container(_, others) =>
          others.exists(b => b.color == color)
        case _ => false
      }.map(_.color)
    }

    override def count(startingColor: String, inputBags: List[Bag]): Int = {
      @tailrec
      def indirect(input: Set[String]): Set[String] = {
        val parents = input.flatMap(c => direct(c, inputBags))

        if (input.size == (parents ++ input).size) {
          input
        } else {
          indirect(input ++ parents)
        }
      }

      val d = direct(startingColor, inputBags)
      indirect(d.toSet).size
    }
  }

  val canFitCounter = new Counter {
    override def count(color: String, input: List[Bag]): Int = {
      def size(bag: Bag, acc: Int): Int = {
        bag match {
          case Container(_, contains) =>
            acc + contains.map(b => size(b, 1)).sum
          case Empty(_) => 1
          case BagRef(color, count) =>
            val childSize = input.filter(_.color == color)
              .map(b => size(b, 1))
              .sum

            count * childSize
        }
      }

      val baseBag = bags.find {
        case Container(name, _) if name == color => true
        case _ => false
      }

      baseBag match {
        case None => 0
        case Some(bag) => size(bag, 0)
      }
    }
  }

  val parser = new BagParser

  val source = fromFile(args(0))

  val bags = source.getLines()
    .map(parser.parse)
    .toList

  source.close()

  val count = containsCounter.count("shiny gold", bags)
  println(count)

  val part2Count = canFitCounter.count("shiny gold", bags)
  println(part2Count)
}
