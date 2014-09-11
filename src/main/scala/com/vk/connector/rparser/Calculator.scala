package com.vk.connector.rparser

import scala.util.{ Failure, Success }
import org.parboiled2._
import scala.annotation.tailrec
import org.parboiled2.ParserInput.apply
import scala.io.StdIn

class Calculator(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule { '(' ~ WhiteSpace ~ ExpressionPlus ~ WhiteSpace ~ ')' | Number }

  def ExpressionPlus = rule {
    '+' ~ WhiteSpace ~ Expression ~ oneOrMore(Expression ~> ((_: Int) + _)) |
    '-' ~ WhiteSpace ~ Expression ~ oneOrMore(Expression ~> ((_: Int) - _)) |
    '*' ~ WhiteSpace ~ Expression ~ oneOrMore(Expression ~> ((_: Int) * _)) |
    '/' ~ WhiteSpace ~ Expression ~ oneOrMore(Expression ~> ((_: Int) / _))
  }

  def Number = rule { capture(Digits) ~ WhiteSpace ~> (_.toInt) }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
}

object Calculator extends App {
  repl()

  @tailrec
  def repl(): Unit =
    StdIn.readLine("---\nEnter expression > ") match {
      case "" =>
      case line =>
        val parser = new Calculator(line)
        parser.InputLine.run() match {
          case Success(result)        => println("Result: " + result)
          case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
          case Failure(e)             => println("Unexpected error during parsing run: " + e)
        }
        repl()
    }
}
