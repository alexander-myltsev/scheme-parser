package com.vk.connector.rparser

import scala.util.{ Failure, Success }
import org.parboiled2._
import scala.annotation.tailrec
import org.parboiled2.ParserInput.apply

class Calculator(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }
  def Expression: Rule1[Int] = rule { zeroOrMore(' ') ~ ExpressionPlus | Factor ~ zeroOrMore(' ') }
  def TrimExpression = rule { ' ' ~ Expression | Expression ~ ' ' }
  def ExpressionPlus: Rule1[Int] = rule {
    '+' ~ Factor ~ oneOrMore(
      Factor ~> ((_: Int) + _)) |
      '-' ~ Factor ~ oneOrMore(
        Factor ~> ((_: Int) - _)) |
        '*' ~ Factor ~ oneOrMore(
          Factor ~> ((_: Int) * _)) |
          '/' ~ Factor ~ oneOrMore(
            Factor ~> ((_: Int) / _)) |
            Factor
  }

  def Factor: Rule1[Int] = rule { zeroOrMore(' ') ~ (Number | Parens) ~ zeroOrMore(' ') }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number = rule { capture(Digits) ~> (_.toInt) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

object Calculator extends App {
  repl()

  @tailrec
  def repl(): Unit =
    readLine("---\nEnter expression > ") match {
      case "" =>
      case line =>
        val parser = new Calculator(line)
        parser.InputLine.run() match {
          case Success(result) => println("Result: " + result)
          case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
          case Failure(e) => println("Unexpected error during parsing run: " + e)
        }
        repl()
    }
}
//new Calculator("1+1").InputLine.run() // evaluates to `scala.util.Success(2)`