package com.us.dsb.explore.types.typeclasses.interpreter

import org.scalatest.FunSpec


/**
  * Interpreter-pattern exploration with `fold` taking per-case functions (to
  * help keep client code synchronized with set of subclasses).
  */
class InterpreterPatternExpl2Test extends FunSpec {


  /** Subthings. */
  object Expr {
    case class IntValue(value: Int) extends Expr
    case class Plus(left: Expr, right: Expr) extends Expr
    case class Times(left: Expr, right: Expr) extends Expr
    case class IntProp(name: String) extends Expr
  }
  import Expr._

  /** Root thing. */
  sealed trait Expr {

    // fold/traverse method case/options:
    // 1. Subthing-specific fns., on unpacked parameters, client must ~recurse.
    // 2. Subthing-specific fns., on whole subthings, client must ~recurse.
    // 3. Single function (on whole subtypes), core handles recursing.
    // x. Optional functions (whole or unpacked), client must ~recurse only for
    //    intercepted cases


    /**
      * Fold version taking per-subthing functions on unwrapped parameter lists.
      * (Non-unique function signatures; but already unpacked for client.)
      */
    def fold1[Result](litFn: Int => Result,
                      plusFn: (Expr, Expr) => Result,
                      timesFn: (Expr, Expr) => Result,
                      propFn: String => Result
                     ): Result = {
      def fold(e: Expr): Result = {
        e match {
          case IntValue(value)    => litFn(value)
          case Plus(left, right)  => plusFn(left, right)
          case Times(left, right) => timesFn(left, right)
          case IntProp(name)      => propFn(name)
        }
      }
      fold(this)
    }

    /**
      * Fold version taking per-subthing functions on still-wrapped subthings.
      * (Unique function signatures, but each clients has to unpack.)
      */
    def fold2[Result](litFn: IntValue => Result,
                      plusFn: Plus => Result,
                      timesFn: Times => Result,
                      propFn: IntProp => Result
                     ): Result = {
      def fold(e: Expr): Result = {
        e match {
          case e: IntValue => litFn(e)
          case e: Plus     => plusFn(e)
          case e: Times    => timesFn(e)
          case e: IntProp  => propFn(e)
        }
      }
      fold(this)
    }


    // determine name ("traverse"? "apply"?);
    def foldX[Result](simplifier: PartialFunction[Expr, Expr]): Expr = {
      def fold(e: Expr): Expr = {
        val subResult =
          e match {
            case e: IntValue => e
            case Plus(left, right) => Plus(fold(left), fold(right))
            case Times(left, right) => Times(fold(left), fold(right))
            case e: IntProp => e
          }
        if (simplifier.isDefinedAt(subResult)) {
          fold(simplifier(subResult))  //????? fold ?
        }
        else {
          subResult
        }
      }
      fold(this)
    }

  }

  /* "Manual" evaluator--using neither fold method. */
  def evaluateDirectly(expr: Expr): Int = {
    expr match {
      case IntValue(v)        => v
      case Plus(left, right)  => evaluateDirectly(left) + evaluateDirectly(right)
      case Times(left, right) => evaluateDirectly(left) * evaluateDirectly(right)
      case IntProp("a")       => 100
    }
  }

  def evalViaFold1(expr: Expr): Int = {
    expr.fold1(
      litFn   = (value: Int) => value,
      plusFn  = (left: Expr, right: Expr) => evalViaFold1(left) + evalViaFold1(right),
      timesFn = (left, right) => evalViaFold1(left) * evalViaFold1(right),
      propFn  = (name) => {name match {case "a" => 100}}
    )
  }

  def evalViaFold2(expr: Expr): Int = {
    expr.fold2(
      litFn   = {case IntValue(value) => value},
      plusFn  = {case Plus(left, right) => evalViaFold2(left) + evalViaFold2(right)},
      timesFn = {case Times(left, right) => evalViaFold2(left) * evalViaFold2(right)},
      propFn  = {case IntProp("a") => 100}
    )
  }

  def format(expr: Expr): String = {
    expr.fold1(
      intValue => intValue.toString,
      (left, right) => s"+(${format(left)}, ${format(right)})",
      (left, right) => s"*(${format(left)}, ${format(right)})",
      (name) => name)
  }


  def simplifyPlusZero1(expr: Expr): Expr = {
    expr.fold1(
      intValue => IntValue(intValue),
      (addend1, addend2) => {
        (addend1, addend2) match {
          case (IntValue(0), addend2) => simplifyPlusZero1(addend2)
          case (addend1, IntValue(0)) => simplifyPlusZero1(addend1)
          case (nonZero1, nonZero2) => Plus(simplifyPlusZero1(nonZero1), simplifyPlusZero1(nonZero2))
        }
      },
      (factor1, factor2) => Times(simplifyPlusZero1(factor1), simplifyPlusZero1(factor2)),
      (propName: String) => IntProp(propName)
    )
  }

  def simplifyPlusZero2(expr: Expr): Expr = {
    expr.fold2(
    identity,
    {
      case Plus(IntValue(0), addend2) => simplifyPlusZero2(addend2)
      case Plus(addend1, IntValue(0)) => simplifyPlusZero2(addend1)
      case Plus(nonZero1, nonZero2) => Plus(simplifyPlusZero2(nonZero1),
                                            simplifyPlusZero2(nonZero2))
    },
    {
      case Times(factor1, factor2) => Times(simplifyPlusZero2(factor1), simplifyPlusZero2(factor2))
    },
    identity
    )
  }

  def simplifyPlusZeroX(expr: Expr): Expr = {
    expr.foldX({
      case Plus(IntValue(0), netRight) => netRight
      case Plus(netLeft, IntValue(0)) => netLeft
    })
  }

  val plusZeroSimplifierfn: PartialFunction[Expr, Expr] = {
    case Plus(IntValue(0), netRight) => netRight
    case Plus(netLeft, IntValue(0)) => netLeft
  }

  val timeOneSimplifierfn: PartialFunction[Expr, Expr] = {
    case Times(IntValue(1), netRight) => netRight
    case Times(netLeft, IntValue(1)) => netLeft
  }

  val incrPlusSimplifier: PartialFunction[Expr, Expr] = {
    case Plus(IntValue(left), IntValue(right)) if left > right => Plus(IntValue(left + 1), IntValue(right - 1))
    case Plus(IntValue(left), IntValue(right)) if left < right => Plus(IntValue(left - 1), IntValue(right + 1))
  }

  def simplifyPlusZeroX1(expr: Expr): Expr = {
    expr.foldX(plusZeroSimplifierfn)
  }

  def simplifyTimesOneX1(expr: Expr): Expr = {
    expr.foldX(timeOneSimplifierfn)
  }

  def simplifyMultipleX1xx(expr: Expr): Expr = {
    expr.foldX(plusZeroSimplifierfn
               orElse timeOneSimplifierfn
                   orElse incrPlusSimplifier
    )
  }


  def printStuff(label: String, expr: Expr): Unit = {
    System.err.println()
    System.err.println(s"$label                           = " + expr)
    System.err.println(s"format($label)                   = " + format(expr))
    System.err.println(s"evaluateDirectly($label)         = " + evaluateDirectly(expr))
    System.err.println(s"evalViaFold1($label)             = " + evalViaFold1(expr))
    System.err.println(s"evalViaFold2($label)             = " + evalViaFold2(expr))
    System.err.println(s"simplifyPlusZero1($label)         = " + simplifyPlusZero1(expr))
    System.err.println(s"format(simplifyPlusZero1($label)) = " + format(simplifyPlusZero1(expr)))
    System.err.println(s"format(simplifyPlusZero2($label)) = " + format(simplifyPlusZero2(expr)))
    //System.err.println(s"format(simplifyPlusZeroX($label)) = " + format(simplifyPlusZeroX(expr)))
    System.err.println(s"format(simplifyTimesOneX1($label)) = " + format(simplifyTimesOneX1(expr)))
    System.err.println(s"format(simplifyMultipleX1($label)) = " + format(simplifyMultipleX1xx(expr)))
    //System.err.println(s"simplify($label)               = " + simplify(expr))
    //System.err.println(s"format(simplify($label))       = " + format(simplify(expr)))
  }


  val expr1: Expr = IntValue(0)
  val expr2a: Expr = Plus(IntValue(1), IntValue(2))
  val expr2b: Expr = Plus(IntValue(0), IntValue(2))
  val expr2c: Expr = Plus(IntValue(1), IntValue(0))
  val expr2d: Expr = Plus(IntValue(0), IntValue(0))
  val expr3: Expr = Plus(IntValue(2), Plus(IntValue(1), IntValue(0)))
  val expr4: Expr = Plus(Plus(IntValue(1), IntValue(0)), IntValue(2))
  val expr5: Expr = Plus(Plus(IntProp("a"), IntValue(0)), Plus(IntProp("a"), IntValue(2)))
  val expr6: Expr = Plus(Plus(IntProp("a"), IntValue(1)), Plus(IntValue(2), IntProp("a")))
  val expr7: Expr = Plus(Times(IntProp("a"), IntValue(1)), Times(IntProp("a"), IntValue(2)))
  val expr8: Expr = Plus(Times(IntProp("a"), IntValue(1)), Plus(IntProp("a"), IntValue(0)))

  it("") {
    //printStuff("expr1", expr1)
    //printStuff("expr2a", expr2a)
    printStuff("expr2b", expr2b)
    printStuff("expr2c", expr2c)
    printStuff("expr2d", expr2d)
    printStuff("expr3", expr3)
    printStuff("expr4", expr4)
    printStuff("expr5", expr5)
    printStuff("expr6", expr6)
    printStuff("expr7", expr7)
    printStuff("expr8", expr8)
  }
}
