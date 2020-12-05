package day2

object Main {
  import scala.io.Source

  case class Policy(min: Int, max: Int, char: Char)

  type Password = String

  trait PasswordVerifier {
    def matches(policy: Policy, password: Password): Boolean
  }

  def parsePolicy(s: String): Policy = {
    val parts = s.split(" ")

    val chars = parts(1).toCharArray
    val bounds = parts(0).split("-").map(_.toInt)

    Policy(bounds(0), bounds(1), chars(0))
  }

  def parseLine(line: String): (Policy, Password) = {
    val parts = line.split(":")
    val password = parts(1)

    val policy = parsePolicy(parts(0))

    (policy, password)
  }

  private val simpleVerifier = new PasswordVerifier {
    override def matches(policy: Policy, password: Password): Boolean = {
      password.count(_ == policy.char) match {
        case x if x >= policy.min && x <= policy.max => true
        case _ => false
      }
    }
  }

  private val advancedVerifier = new PasswordVerifier {
    override def matches(policy: Policy, password: Password): Boolean = {
      val firstMatches = password.charAt(policy.min) == policy.char
      val secondMatches = password.charAt(policy.max) == policy.char

      (firstMatches && !secondMatches) || (secondMatches && !firstMatches)
    }
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("dayTwoInput.txt", "UTF-8")

    val lines = source.getLines().toList
    source.close()


    val answer = lines
      .map(parseLine)
      .count {
        case (x, y) => advancedVerifier.matches(x, y)
      }

    println(answer)
  }
}
