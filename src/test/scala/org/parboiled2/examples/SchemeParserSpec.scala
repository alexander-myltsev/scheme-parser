package org.parboiled2.examples

import org.scalatest.{WordSpec, Matchers}
import scala.util.Success

class SchemeParserSpec extends WordSpec
                          with Matchers {
  "SchemeParser" should {
    "parse number" in {
      new SchemeParser("42").InputLine.run() shouldBe Success(42)
    }
  }
}
