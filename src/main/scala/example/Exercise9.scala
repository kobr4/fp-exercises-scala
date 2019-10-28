package example

import example.Exercise8.Exercise8_13
import example.Exercise8.Exercise8_13.Prop
import example.Exercise8.Exercise8_4.Gen
import example.JSON._

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
  }

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))((a, l) => a +: l) or succeed(List.empty[A])

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))((a, l) => a +: l)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = productFM(p, p2)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map[C](f.tupled)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  @tailrec
  final def listOfN[A](n: Int, p: Parser[A], in: Parser[List[A]] = succeed(List.empty[A])): Parser[List[A]] = {
    if (n == 0)
      in
    else listOfN(n - 1, p, map2(p, in)((a, l) => a +: l))
  }

  def contextSensitive(): Parser[String] = flatMap("[0-9]".r)(_ => "[a-z]*".r)

  def productFM[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    flatMap(p)(a => flatMap(p2)(b => succeed(a, b)))
  }

  def map2FM[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    flatMap(p)(a => flatMap(p2)(b => succeed(f(a, b))))
  }

  def mapFM[A, B](a: Parser[A])(f: A => B): Parser[B] = {
    flatMap(a)(resA => succeed(f(resA)))
  }


  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Exercise8_13.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def productLaw[A](p: Parser[A], in: Gen[String]): Prop =
      equal(p, product(p, p).map(_._1))(in)
  }

}

sealed trait JSON


object JSON {

  case object JNULL extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON


}

case class ParseErrorImpl(message: String)

import fastparse._

case class ParserImpl(p: P[_])

object Test {

  import fastparse._
  import NoWhitespace._

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  def matchString[_: P](s: String): P[String] = P(s).map(Unit => s)

  type ParserImpl[+A] = P[_] => P[A]
}

case class RealParser() extends Parsers[ParseErrorImpl, Test.ParserImpl] {

  override def run[A](p: Test.ParserImpl[A])(input: String): Either[ParseErrorImpl, A] = {
    parse[A](input, p) match {
      case Parsed.Success(value, successIndex) => Right(value)
      case Parsed.Failure(label, index, extra) => Left(ParseErrorImpl(s"ERROR INDEX:${index}"))
    }
  }

  override def or[A](s1: Test.ParserImpl[A], s2: Test.ParserImpl[A]): Test.ParserImpl[A] =
    (ctxt) => {

      val resS1 = s1(ctxt)

      if (resS1.isSuccess) resS1 else s2(ctxt)
    }

  override implicit def string(s: String): Test.ParserImpl[String] = (ctxt) => Test.matchString(s)(ctxt)

  override implicit def regex(r: Regex): Test.ParserImpl[String] = (ctxt) => {
    r.findPrefixOf(ctxt.input.slice(ctxt.index, ctxt.input.length).toString) match {
      case scala.None => Fail(ctxt)
      case scala.Some(s) => string(s)(ctxt)
    }
  }

  override def map[A, B](a: Test.ParserImpl[A])(f: A => B): Test.ParserImpl[B] = {
    (ctxt) => a(ctxt).map(f)
  }

  override def flatMap[A, B](p: Test.ParserImpl[A])(f: A => Test.ParserImpl[B]): Test.ParserImpl[B] =
    (ctxt) => p(ctxt).flatMapX(a => f(a)(ctxt))

  override def slice[A](p: Test.ParserImpl[A]): Test.ParserImpl[String] = (ctxt) => p(ctxt).map(a => a.toString)

}


object Exercise9 {

  val realParser = RealParser()

  import realParser._

  val quote = realParser.char('"')

  val comma = realParser.char(',')

  val colon = realParser.char(':')

  val alphaNum = realParser.regex("[a-zA-Z0-9]*".r)

  val num = realParser.regex("[0-9]+".r)

  val sqBracketOpen = realParser.char('[')

  val sqBracketClose = realParser.char(']')

  val braceOpen = realParser.char('{')

  val braceClose = realParser.char('}')

  val JsBool = realParser.string("true").map(_ => JBool(true)) or realParser.string("false").map(_ => JBool(false))

  val JsNum = num.map(i => JNumber(i.toInt))

  val JsString = quotedName.map(s => JString(s))

  lazy val quotedName = for {
    _ <- Exercise9.quote
    alpha <- Exercise9.alphaNum
    _ <- Exercise9.quote
  } yield {
    alpha
  }

  val valueName = for {
    name <- quotedName
    _ <- colon
    v <- JsNum | JsBool | JsArray | JsObj
  } yield {
    (name, v)
  }

  val JsArray = for {
    _ <- sqBracketOpen
    listElt <- many(withComma(JsString) | withComma(JsBool) | withComma(JsNum))
    _ <- sqBracketClose
  } yield {
    JArray(listElt.toIndexedSeq)
  }

  def withComma[A](p: Test.ParserImpl[A]) = for {
    result <- p
    _ <- comma | succeed("")
  } yield {
    result
  }

  val JsObj: Test.ParserImpl[JObject] = for {
    _ <- braceOpen
    listElt <- many(withComma(valueName))
    _ <- braceClose
  } yield {
    JObject(listElt.toMap)
  }

}
