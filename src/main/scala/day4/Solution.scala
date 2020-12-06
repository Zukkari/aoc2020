package day4

import day4.Solution.Passport

import scala.annotation.tailrec
import scala.io.Source.fromFile
import scala.util.matching.Regex

trait PassportValidator {
  def isValid(p: Solution.Passport): Boolean
}

trait FieldValidator {
  def isValid(p: Solution.Passport): Boolean
}

case class Bounds(min: Int, max: Int) {
  def fits(in: Int): Boolean = in >= min && in <= max
}

case class NumberValidator(key: String, len: Int, bounds: Bounds) extends FieldValidator {
  override def isValid(p: Passport): Boolean = {
    p.get(key) match {
      case Some(v) if v.length == len => bounds.fits(v.toInt)
      case _ => false
    }
  }
}

case object HeightValidator extends FieldValidator {
  private val cmBounds = Bounds(150, 193)
  private val inBounds = Bounds(59, 76)

  private def cmValid(s: String): Boolean = {
    val intValue = s.substring(0, s.length - 2).toInt
    cmBounds.fits(intValue)
  }

  private def inValid(s: String): Boolean = {
    val intValue = s.substring(0, s.length - 2).toInt
    inBounds.fits(intValue)
  }

  override def isValid(p: Passport): Boolean = {
    p.get("hgt") match {
      case Some(v) => v match {
        case height if height.endsWith("cm") => cmValid(height)
        case height if height.endsWith("in") => inValid(height)
        case _ => false
      }
      case _ => false
    }
  }
}

case class PatternValidator(key: String, pattern: Regex) extends FieldValidator {
  override def isValid(p: Passport): Boolean = {
    p.get(key) match {
      case Some(v) => pattern.matches(v)
      case _ => false
    }
  }
}

object Solution extends App {
  type Passport = Map[String, String]

  def parseLine(line: String, current: Passport): Passport =
    line.split(" ")
      .map { x =>
        val parts = x.split(":")
        (parts(0), parts(1))
      }
      .toMap ++ current

  def parse(lines: List[String]): List[Passport] = {
    @tailrec
    def parse(lines: List[String], acc: List[Passport], current: Passport): List[Passport] = {
      lines.headOption match {
        case None => current :: acc
        case Some(line) if (line.isEmpty) => parse(lines.tail, current :: acc, Map.empty)
        case Some(line) =>
          val newPassport = parseLine(line, current)
          parse(lines.tail, acc, newPassport)
      }
    }

    parse(lines, Nil, Map.empty)
  }

  val file = args(0)

  val source = fromFile(file)

  val lines = source.getLines().toList

  val passports = parse(lines)

  val validator: PassportValidator = new PassportValidator {
    val mandatoryFields: List[FieldValidator] = List(
      NumberValidator("byr", 4, Bounds(1920, 2002)),
      NumberValidator("iyr", 4, Bounds(2010, 2020)),
      NumberValidator("eyr", 4, Bounds(2020, 2030)),
      HeightValidator,
      PatternValidator("hcl", "#[0-9a-f]{6}".r),
      PatternValidator("ecl", "amb|blu|brn|gry|grn|hzl|oth".r),
      PatternValidator("pid", "[0-9]{9}".r)
    )

    override def isValid(p: Passport): Boolean = {
      mandatoryFields.forall(_.isValid(p))
    }
  }

  val answer = passports.count(validator.isValid)
  println(answer)
}
