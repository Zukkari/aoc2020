package day10

import scala.annotation.tailrec
import scala.io.Source

case class Jolt(v: Int)

case class Adapter(base: Int)

case class Connector(in: Adapter, jolt: Jolt, out: Adapter)

object Solution extends App {
  def chain(adapters: List[Int], jolts: List[Jolt]): List[Connector] = {
    @tailrec
    def chain0(
        curr: Int,
        adapters: List[Int],
        acc: List[Connector]
    ): List[Connector] = {
      adapters match {
        case Nil => acc
        case x :: xs =>
          val m = jolts
            .map(jolt => (jolt.v + curr, jolt))
            .find(_._1 == x)

          m match {
            case None => acc
            case Some((_, jolt)) =>
              val base = Adapter(curr)
              val next = Adapter(x)
              val connection = Connector(base, jolt, next)
              chain0(x, xs, connection :: acc)
          }
      }
    }

    val res = chain0(0, adapters, Nil)
    val last = res.head

    val extended = Adapter(last.out.base + 3)

    Connector(last.out, Jolt(3), extended) :: res
  }

  val jolts = List(
    Jolt(1),
    Jolt(2),
    Jolt(3)
  )

  val s = Source.fromFile(args(0))
  val adapters = s.getLines().map(_.toInt).toList.sorted
  s.close()

  val adapterChain = chain(adapters, jolts)

  val diffOne = adapterChain.count(_.jolt.v == 1)
  val diffThree = adapterChain.count(_.jolt.v == 3)

  val answer = diffOne * diffThree
  println(answer)
}
