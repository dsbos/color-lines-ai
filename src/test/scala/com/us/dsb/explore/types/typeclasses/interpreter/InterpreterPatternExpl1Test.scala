package com.us.dsb.explore.types.typeclasses.interpreter

import org.scalatest.funspec.AnyFunSpec


/**
  * Interpreter-pattern exploration with just independent interpreters (e.g.,
  * no common `fold` method to help keep client code synchronized with set of
  * subclasses.)
  */
class InterpreterPatternExpl1Test extends AnyFunSpec {

  sealed trait Expr

  case class IntValue(value: Int) extends Expr
  case class IntProp(name: String) extends Expr

  sealed abstract class BinaryOp extends Expr
  object BinaryOp {
    def unapply(e: Expr): Option[(Expr, Expr)] = {
      e match {
        case Plus(left, right) => Some((left, right))
        case Times(left, right)  => Some((left, right))
        case x: BinaryOp =>
          System.err.println("x  = " + x)
          ???  // something new?
        case _                   => None
      }
    }
  }
  case class Plus(left: Expr, right: Expr) extends BinaryOp
  case class Times(left: Expr, right: Expr) extends BinaryOp

  sealed abstract class TernaryOp extends Expr
  object TernaryOp {
    def unapply(e: Expr): Option[(Expr, Expr, Expr)] = {
      e match {
        case IfPositive(value, ifPos, isNonPos)  => Some((value, ifPos, isNonPos))
        case _                  => None
      }
    }
  }
  case class IfPositive(value: Expr, ifPos: Expr, isNonPos: Expr) extends TernaryOp


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

  val expr11: Expr = IfPositive(IntValue(2), IntValue(1), IntValue(-10))
  val expr12: Expr = IfPositive(IntValue(-2), IntValue(1), IntValue(-10))
  val expr13: Expr = IfPositive(IntProp("a"), IntProp("b"), IntProp("c"))

  def evaluate(expr: Expr): IntValue = {
    val value =
      expr match {
        case IntValue(v)         => v
        case Plus(left, right) => evaluate(left).value + evaluate(right).value
        case Times(left, right)  => evaluate(left).value * evaluate(right).value
        case IntProp("a")        => 100
        case IntProp("b")        => 200
        case IntProp("c")        => 400
        case IfPositive(cond, ifPos, ifNonPos) =>
          if (evaluate(cond).value > 0) evaluate(ifPos).value else evaluate(ifNonPos).value
        case _ => fail()
      }
    IntValue(value)
  }

  def format(expr: Expr): String = {
    expr match {
      case IntValue(value)  => s"$value"
      case Plus(left, right) => s"+( ${format(left)}, ${format(right)} )"
      case Times(left, right) => s"*( ${format(left)}, ${format(right)} )"
      case IntProp(name)    => s"$name"
      case IfPositive(cond, ifPos, ifNonPos) => s"if( ${format(cond)}, ${format(ifPos)}, ${format(ifNonPos)} )"
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
                    | _: Times
                    | _: IfPositive) => other
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

      case Times(left, right)      => Times(simplify(left), simplify(right))

      case IfPositive(cond, ifPos, ifNonPos) =>
        simplify(cond) match {
          case IntValue(pos) if pos > 0 => simplify(ifPos)
          case IntValue(_)              => simplify(ifNonPos)
          case nonValue                 =>
            IfPositive(nonValue, simplify(ifPos), simplify(ifNonPos) )
        }
    }
  }

  def listProperties(expr: Expr): Set[String] = {
    val value: Set[String] =
      expr match {
        case IntProp(name)      => Set(name)
        case IntValue(_)        => Set[String]()
        case BinaryOp(left, right) => listProperties(left) ++ listProperties(right)
        //case Plus(left, right)  => listProperties(left) ++ listProperties(right)
        //case Times(left, right) => listProperties(left) ++ listProperties(right)
        case IfPositive(c, p, n) => List(c, p, n).map(listProperties(_)).flatten.toSet
        case _ => fail()
      }
    value
  }



  def nameThis(label: String, expr: Expr): Unit = {
    System.err.println()
    System.err.println(s"$label                   = " + expr)
    System.err.println(s"format($label)           = " + format(expr))
    System.err.println(s"evaluate($label)         = " + evaluate(expr))
    System.err.println(s"simplifyPlusZero($label) = " + simplifyPlusZero(expr))
    System.err.println(s"simplify($label)         = " + simplify(expr))
    System.err.println(s"format(simplify($label)) = " + format(simplify(expr)))
    System.err.println(s"listProperties(simplify($label)) = " + listProperties(expr))
    assert(evaluate(expr) == evaluate(simplify(expr)))
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

  nameThis("expr11", expr11)
  nameThis("expr12", expr12)
  nameThis("expr13", expr13)

  ////////////////////////////
  //????????????
/*

  //??: Interpreter?  (Interpretable?)
  trait Dumpable {
    // Some operation specific to type class:
    def interpret(e: Expr): String
  }

  // Client of type class and operation(?):
  def interpret(t: Expr)(implicit d: Dumpable) = d.interpret(t)



  // Type class instance of Dumpable for ThingThing:
  implicit object DumpableThingOnexx extends Dumpable {
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
