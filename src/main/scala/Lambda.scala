// Lambda calculus with integers and if

import stainless.annotation._
import stainless.lang._
import stainless.lang.Map._
object Lambda {
  @extern
  def printInteger(i: Int) : String = i.toString

  case class Environment(m: String => Option[Value]) {
    def update(newVar: String, newValue: Value) = Environment((s: String) =>
      if (s==newVar) Some(newValue)
      else m(s)
    )
  }

  sealed abstract class Expression {
    override def toString: String = this match {
      case Const(c) => printInteger(c)
      case Var(s) => s
      case Apply(f, arg) => f.toStringPar + "(" + arg.toString + ")"
      case Lambda(bvar, body) => bvar + "=> " + body.toString
      case If(cond,thenB,elseB) => "if (" + cond.toString + ") " +
        thenB.toStringPar + " else " + elseB.toString
    }
    def toStringPar: String = this match {
      case Const(c) => toString + " "
      case Lambda(_,_) => "(" + toString + ")"
      case If(_,_,_) => "(" + toString + ")"
      case _ => toString
    }
  }
  case class Const(c: Int) extends Expression
  case class Var(s: String) extends Expression
  case class If(cond: Expression, thenB: Expression, elseB: Expression) extends Expression
  case class Apply(f: Expression, arg: Expression) extends Expression
  case class Lambda(boundVar: String, body: Expression) extends Expression

  sealed abstract class Value {
    override def toString: String = this match {
      case IntegerValue(c) => printInteger(c)
      case FunctionValue(f) => "<function>"
      case Err(msg) => "Error! " + msg
    }
  }
  case class IntegerValue(c: Int) extends Value
  case class FunctionValue(f: Value => Value) extends Value
  case class Err(msg: String) extends Value

  def selfApp(f: Expression) =
    Lambda("x", Apply(f,Apply(Var("x"), Var("x"))))
  def Y(f: Expression) =
    Apply(selfApp(f), selfApp(f))
  def yf = Y(Var("f")).toString

  def eval(expr: Expression, env: Environment) : Value = {
      expr match {
        case Const(c) => IntegerValue(c)
        case Var(s) => env.m(s) match {
            case Some(v) => v
            case None() => Err("Unknown variable " + s + ".") }
        case If(cond, thenB, elseB) => eval(cond, env) match {
          case IntegerValue(0) => eval(elseB, env)
          case IntegerValue(_) => eval(thenB, env)
          case _ => Err("Condition of if not an integer.")
        }
        case Apply(fexpr, arg) => eval(fexpr, env) match {
            case FunctionValue(f) => f(eval(arg, env))
            case _ => Err("Application of " + fexpr.toString +
                          " evaluated to value that is not a function.")
        }
        case Lambda(bv, body) => FunctionValue(
            (v: Value) => eval(body, env.update(bv, v))
        )
      }
  }

  def lift(op: (Int, Int) => Int) : Value = FunctionValue((v1: Value) => v1 match {
    case IntegerValue(c1) => FunctionValue((v2: Value) => v2 match {
      case IntegerValue(c2) => IntegerValue(op(c1, c2))
      case _ => Err("Plus given a non-integer value as second argument.")
    })
    case _ => Err("Plus given a non-integer value as first argument.")
  })

  def makeEnv(m: Map[String, Value]) = Environment(m.get(_))
  def lessThan(x: Int, y: Int): Int = if (x < y) 1 else 0
  def stdEnv = makeEnv(Map(
    "+" -> lift(_ + _),
    "-" -> lift(_ - _),
    "*" -> lift(_ * _),
    "<" -> lift(lessThan)))

  def mul3: Expression = Lambda("x",
    Apply(Apply(Var("*"), Const(3)), Var("x")))

  def expr3x8: Expression = Apply(mul3, Const(8))
  def res3x8: Value = eval(expr3x8, stdEnv)

  def absExpr: Expression = Lambda("x",
    If(Apply(Apply(Var("<"), Const(0)), Var("x")),
       Var("x"),
       Apply(Apply(Var("-"), Const(0)), Var("x"))
      )
  )
  def absEx1: Expression = Apply(absExpr, Const(-65))
  def absEx2: Expression = Apply(absExpr, Const(-7))
  def absExRes = eval(absEx2, stdEnv)
}
