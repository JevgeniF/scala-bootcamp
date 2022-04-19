package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.ControlStructuresHomework2.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source
import scala.language.postfixOps

object ControlStructuresHomework2 {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  final case class Result(comm: Command, res: Double)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val inputList = x.split(" ").map(_.trim)
    val command = inputList.head.toLowerCase()
    val numbers = try inputList.tail.map(x => x.toDouble).toList
    catch { case _: Exception => return Left(ErrorMessage(x))}

    if (numbers.isEmpty) return Left(ErrorMessage(x))

    command match {
      case command if command == "divide" && numbers.length == 2 => Right(Divide(numbers.head, numbers(1)))
      case command if command  == "sum"  => Right(Sum(numbers))
      case command if command == "average" => Right(Average(numbers))
      case command if command == "min" => Right(Min(numbers))
      case command if command == "max" => Right(Max(numbers))
      case _ => Left(ErrorMessage(x))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(dividend, divisor) => if (divisor != 0)
        Right(Result(x, dividend / divisor)) else Left(ErrorMessage("Division by zero is prohibited"))
      case Sum(numbers) => Right(Result(x, numbers.sum))
      case Average(numbers) => Right(Result(x, numbers.sum / numbers.length))
      case Min(numbers) => Right(Result(x, numbers.min))
      case Max(numbers) => Right(Result(x, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    x.comm match {
      case Divide(dividend, divisor) => s"$dividend divided by $divisor is ${x.res}"
      case Sum(numbers) => s"the sum of ${numbers.mkString(" ")} is ${x.res}"
      case Average(numbers) => s"the average of ${numbers.mkString(" ")} is ${x.res}"
      case Min(numbers) => s"the minimum of ${numbers.mkString(" ")} is ${x.res}"
      case Max(numbers) => s"the maximum of ${numbers.mkString(" ")} is ${x.res}"
      case x.comm => "Unexpected!"
    }
  }

  def process(x: String): String = {
    val rendered = for {
      comm <- parseCommand(x)
      res <- calculate(comm)
      ren = renderResult(res)
    } yield ren
    rendered.fold(left => s"Error: ${left.value}", right => right)
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
