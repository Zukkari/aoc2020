package day9

import day9.Solution.Cypher

import scala.annotation.tailrec
import scala.io.Source

trait CypherValidator {
  def isValid(cypher: Cypher): (Boolean, Long)
}

class CypherValidatorImpl(val keyLength: Int) extends CypherValidator {

  @tailrec
  final override def isValid(cypher: Cypher): (Boolean, Long) = {
    val (key, rest) = cypher.splitAt(keyLength)
    rest match {
      case x :: _ =>
       val headValid = key.combinations(2)
        .exists(_.sum == x)

        if (headValid) {
          isValid(key.tail ++ rest)
        } else {
          (false, x)
        }
      case Nil => (true, 0)
    }
  }
}

trait EncryptionBreaker {
  def break(cypher: Cypher, target: Long): Seq[Iterable[Long]]
}

class EncryptionBreakerImpl extends EncryptionBreaker {
  override def break(cypher: Cypher, target: Long): Seq[Iterable[Long]] = {
    @tailrec
    def break(cypher: Cypher, window: Int): Option[Iterable[Long]] = {
      cypher match {
        case Nil => None
        case _ =>
          val (key, rest) = cypher.splitAt(window)
          if (key.sum == target) {
            Some(key)
          } else {
            break(key.tail ++ rest, window)
          }
      }
    }

    for {
      window <- 2 to cypher.length
      seq <- {
        println(s"Trying window size $window for target $target")
        break(cypher, window)
      }
    } yield seq
  }
}

object Solution extends App {
  type Cypher = List[Long]

  val s = Source.fromFile(args(0))
  val lines = s.getLines().map(_.toLong).toList
  s.close()

  val preamble = 25

  val cypherValidatorImpl = new CypherValidatorImpl(preamble)

  val v = cypherValidatorImpl.isValid(lines)
  println(v)

  val x = new EncryptionBreakerImpl
  val answer = x.break(lines, v._2)
  println(answer
  .map(x => {
    val s = x.toList.sorted
    s.head + s.last
  }))
}
