package org.parboiled2.examples

import scala.util.{Failure, Success}
import org.parboiled2._
import scala.annotation.tailrec

object SchemeParser extends App {
  repl()

  @tailrec
  def repl(): Unit =
    readLine("---\nEnter expression > ") match {
      case "" =>
      case line =>
        val parser = new SchemeParser(line)
        parser.InputLine.run() match {
          case Success(result)        => println("Result: " + result)
          case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
          case Failure(e)             => println("Unexpected error during parsing run: " + e)
        }
        repl()
    }
}

class SchemeParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule { capture("42") ~> ((_:String).toInt) }
}
