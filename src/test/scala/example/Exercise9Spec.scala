package example

import example.JSON.{JArray, JBool, JNumber, JObject}
import org.scalatest.{FlatSpec, Matchers}
class Exercise9Spec extends FlatSpec with Matchers {

  "Exercise9" should "parse" in {

    val rp = RealParser()

    val hook = rp.char('[')

    rp.run(hook)("[") shouldBe Right('[')

    val toto = rp.regex("toto".r)

    rp.run(toto)("toto") shouldBe Right("toto")

    rp.run(toto)("tititoto") shouldBe a[Left[_]]

    val titi = rp.regex("titi".r)

    val tototiti = rp.flatMap(toto)((_) => titi)

    rp.run(tototiti)("tototiti") shouldBe Right("titi")

    rp.run(Exercise9.quotedName)("\"toto123\"") shouldBe Right("toto123")

  }

  "Exercise9" should "not recognize a number" in {

    Exercise9.realParser.run(Exercise9.num)("") shouldBe a[Left[_]]
  }

  "Exercise9" should "recognize a JBOOL" in {

    Exercise9.realParser.run(Exercise9.JsBool)("true") shouldBe Right(JBool(true))

    Exercise9.realParser.run(Exercise9.JsBool)("false") shouldBe Right(JBool(false))

  }

  "Exercise9" should "recognize a named value" in {

    Exercise9.realParser.run(Exercise9.valueName)("\"toto123\":42") shouldBe Right(("toto123", JNumber(42)))
  }

  "Exercise9" should "recognize an array" in {

    Exercise9.realParser.run(Exercise9.JsArray)("[]") shouldBe Right(JArray(IndexedSeq()))

    Exercise9.realParser.run(Exercise9.JsArray)("""[1,2,3]""") shouldBe
      Right(JArray(IndexedSeq(JNumber(1), JNumber(2), JNumber(3))))
  }

  "Exercise9" should "recognize an json object" in {

    Exercise9.realParser.run(Exercise9.JsObj)("""{"toto123":42,"array":[1,2,3]}""") shouldBe
      Right(JObject(Map("toto123" -> JNumber(42),"array" -> JArray(IndexedSeq(JNumber(1), JNumber(2), JNumber(3))))))
  }
}
