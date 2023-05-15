package playground

import zoo.Fix
import zoo.Functor
import zoo.Functor.Syntax._
import zoo.Schemes._

sealed trait ExprF[A]

object ExprF {
  final case class Literal[A](intVal: Int) extends ExprF[A]
  final case class Ident[A](name: String) extends ExprF[A]
  final case class Unary[A](op: String, target: A) extends ExprF[A]
  final case class Binary[A](op: String, lhs: A, rhs: A) extends ExprF[A]
  final case class Call[A](func: A, args: List[A]) extends ExprF[A]
  final case class Paren[A](target: A) extends ExprF[A]

  def literal[A](i: Int): ExprF[A] = Literal(i)
  def ident[A](n: String): ExprF[A] = Ident(n)
  def unary[A](op: String, tgt: A): ExprF[A] = Unary(op, tgt)
  def binary[A](op: String, lhs: A, rhs: A): ExprF[A] = Binary(op, lhs, rhs)
  def call[A](f: A, args: List[A]): ExprF[A] = Call(f, args)
  def paren[A](tgt: A): ExprF[A] = Paren(tgt)

  def applyExprF[A, B](f: A => B)(expr: ExprF[A]): ExprF[B] = ???

  implicit val exprFunctor = new Functor[ExprF] {
    override def map[A, B](f: A => B)(expr: ExprF[A]): ExprF[B] = expr match {
      // base cases
      case Literal(i) => Literal(i)
      case Ident(n) => Ident(n)

      // recursive cases
      case Paren(tgt) => Paren(f(tgt))
      case Unary(op, arg) => Unary(op, f(arg))
      case Binary(op, l, r) => Binary(op, f(l), f(r))
      case Call(e, args) => Call(f(e), args.map(f))
    }
  }

  type NestedExpr3 = ExprF[ExprF[ExprF[Literal[Int]]]]
  type ExprFixed = Fix[ExprF]

  val ten: Fix[ExprF] = Fix(literal(10))
  val add: Fix[ExprF] = Fix(ident("add"))
  val addTen: Fix[ExprF] = Fix(call(add, List(ten, ten)))
  val multInParens: ExprFixed = Fix(paren(Fix(paren(Fix(binary("*", ten, ten))))))

  def bottomUp[F[_]: Functor](f: Fix[F] => Fix[F])(term: Fix[F]): Fix[F] = {
    val unpacked: F[Fix[F]] = term.unfix
    val recurseWith: Fix[F] => Fix[F] = bottomUp(f)
    val recursed: F[Fix[F]] = unpacked.map(recurseWith)
    val repacked: Fix[F] = Fix(recursed)
    val applied: Fix[F] = f(repacked)
    applied 
  }

  def flattenExpr(expr: ExprFixed): ExprFixed = expr match {
    case Fix(Paren(tgt)) => tgt
    case other => other
  }
  
  def flatten(expr: ExprFixed): ExprFixed =
    bottomUp(flattenExpr)(expr)

  // ((10 * 10)) = 10 * 10
  val flattened = flatten(multInParens)

  def countNodes: ExprF[Int] => Int = {
    case Literal(_) => 1
    case Ident(_) => 1
    case Unary(_, arg) => arg +1
    case Binary(_, left, right) => left + right +1
    case Call(fn, args) => fn + args.sum + 1
    case Paren(tgt) => tgt + 1
  }

  def prettyPrint: Algebra[ExprF, String] = {
    case Literal(i) => f"$i%d"
    case Ident(n) => f"$n%s"
    case Call(fn, args) => s"""$fn${args.mkString("(", ",", ")")}"""
    case Unary(op, arg) => s"$op$arg"
    case Binary(op, l, r) => s"$l $op $r"
    case Paren(exp) => s"($exp)"
  }

  val counted = mystery(countNodes)(addTen)
  val printed = cata(prettyPrint)(addTen)

  def nested(n: Int): Fix[ExprF] = {
    def go: Coalgebra[ExprF, Int] = {
      case 0 => literal(n)
      case i => paren(i - 1)
    }
    ana(go)(n)
  }

  def parens(n: Int): Coalgebra[ExprF, Int] = {
    case 0 => literal(n)
    case i => paren(i - 1)
  }

  val nested3AndPrinted = hylo[ExprF, Int, String](prettyPrint)(parens(0)).apply(3)
 
}
