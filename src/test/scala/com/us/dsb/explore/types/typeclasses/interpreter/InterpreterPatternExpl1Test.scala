package com.us.dsb.explore.types.typeclasses.interpreter

import org.scalatest.FunSpec


/**
  * Interpreter-pattern exploration with just independent interpreters (e.g.,
  * no common `fold` method to help keep client code synchronized with set of
  * subclasses.)
  */
class InterpreterPatternExpl1Test extends FunSpec {

  sealed trait Expr
  case class IntValue(value: Int) extends Expr
  case class Plus(left: Expr, right: Expr) extends Expr
  case class Times(left: Expr, right: Expr) extends Expr
  case class IntProp(name: String) extends Expr

  val expr1: Expr = IntValue(0)
  val expr2a: Expr = Plus(IntValue(1), IntValue(2))
  val expr2b: Expr = Plus(IntValue(0), IntValue(2))
  val expr2c: Expr = Plus(IntValue(1), IntValue(0))
  val expr2d: Expr = Plus(IntValue(0), IntValue(0))
  val expr3: Expr = Plus(IntValue(2), Plus(IntValue(1), IntValue(0)))
  val expr4: Expr = Plus(Plus(IntValue(1), IntValue(0)), IntValue(2))
  val expr5: Expr = Plus(Plus(IntProp("a"), IntValue(0)), Plus(IntProp("a"), IntValue(2)))
  val expr6: Expr = Plus(Plus(IntProp("a"), IntValue(1)), Plus(IntValue(2), IntProp("a")))

  val expr7: Expr = Plus(Times(IntProp("a"), IntValue(3)), Times(IntProp("a"), IntValue(2)))


  def evaluate(expr: Expr): IntValue = {
    val value =
      expr match {
        case IntValue(v)      => v
        case Plus(left, right) => evaluate(left).value + evaluate(right).value
        case Times(left, right) => evaluate(left).value * evaluate(right).value
        case IntProp("a")     => 100
      }
    IntValue(value)
  }

  def format(expr: Expr): String = {
    expr match {
      case IntValue(value)  => s"$value"
      case Plus(left, right) => s"+( ${format(left)}, ${format(right)} )"
      case Times(left, right) => s"*( ${format(left)}, ${format(right)} )"
      case IntProp(name)    => s"$name"
    }
  }


  def simplifyPlusZero(expr: Expr): Expr = {
    expr match {
      case Plus(left, right) =>
        Plus(simplifyPlusZero(left),
            simplifyPlusZero(right)) match {
          case Plus(IntValue(0), netRight) => netRight
          case Plus(netLeft, IntValue(0))  => netLeft
          case otherPlus: Plus              => otherPlus
        }
      case other @ (_: IntValue
                    | _: IntProp
                    | _: Times) => other
    }
  }

  def simplify(expr: Expr): Expr = {
    expr match {
      case Plus(Times(common1, x1), Times(common2, x2)) if common1 == common2 =>
        Times(common1, Plus(x1, x2))

      case value: IntValue => value
      case prop: IntProp => prop
      case Plus(IntValue(0), arg) => simplify(arg)
      case Plus(arg, IntValue(0)) => simplify(arg)
      case Plus(left: IntValue, right: IntProp) => simplify(Plus(right, left))
      case Plus(left, right)                    => Plus(simplify(left), simplify(right))

      case Times(zero @ IntValue(0), _) => zero
      case Times(_, zero @ IntValue(0)) => zero
      case Times(IntValue(1), arg) => simplify(arg)
      case Times(arg, IntValue(1)) => simplify(arg)

      case Times(left, right)                   => Times(simplify(left), simplify(right))

    }
  }

  def nameThis(label: String, expr: Expr): Unit = {
    System.err.println()
    System.err.println(s"$label                   = " + expr)
    System.err.println(s"format($label)           = " + format(expr))
    System.err.println(s"evaluate($label)         = " + evaluate(expr))
    System.err.println(s"simplifyPlusZero($label) = " + simplifyPlusZero(expr))
    System.err.println(s"simplify($label)         = " + simplify(expr))
    System.err.println(s"format(simplify($label)) = " + format(simplify(expr)))
  }


  nameThis("expr1", expr1)
  nameThis("expr2a", expr2a)
  nameThis("expr2b", expr2b)
  nameThis("expr2c", expr2c)
  nameThis("expr2d", expr2d)
  nameThis("expr3", expr3)
  nameThis("expr4", expr4)
  nameThis("expr5", expr5)
  nameThis("expr6", expr6)
  nameThis("expr7", expr7)


  ////////////////////////////
  //????????????
/*

  //??: Interpreter?  (Interpretable?)
  trait Dumpablexx {
    // Some operation specific to type class:
    def interpret(e: Expr): String
  }

  // Client of type class and operation(?):
  def interpret(t: Expr)(implicit d: Dumpablexx) = d.interpret(t)



  // Type class instance of Dumpable for ThingThing:
  implicit object DumpableThingOnexx extends Dumpablexx {
    def interpret(t: Expr): String = {
      t.toString
    }
  }




  // interpreter 1: evaluate:


  // interpreter 2: display
*/
  it(""){

  }
}
