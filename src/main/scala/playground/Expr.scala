package playground

sealed trait Expr

object Expr {
  final case class Literal(intVal: Int) extends Expr
  final case class Ident(name: String) extends Expr
  final case class Unary(op: String, target: Expr) extends Expr
  final case class Binary(op: String, lhs: Expr, rhs: Expr) extends Expr
  final case class Call(func: Expr, args: List[Expr]) extends Expr
  final case class Paren(target: Expr) extends Expr

  def flatten(expr: Expr): Expr = expr match {
    case Literal(i) => Literal(i)
    case Ident(n) => Ident(n)
    case Paren(tgt) => flatten(tgt)
    case Unary(op, arg) => Unary(op, flatten(arg))
    case Binary(op, l, r) => Binary(op, flatten(l), flatten(r))
    case Call(e, args) => Call(flatten(e), args.map(flatten))
  }

  def applyExpr(f: Expr => Expr)(expr: Expr): Expr = expr match {
    // base cases
    case Literal(i) => Literal(i)
    case Ident(n) => Ident(n)

    // recursive cases
    case Paren(tgt) => Paren(f(tgt))
    case Unary(op, arg) => Unary(op, f(arg))
    case Binary(op, l, r) => Binary(op, f(l), f(r))
    case Call(e, args) => Call(f(e), args.map(f))
  }

  def flatten2(expr: Expr): Expr = expr match {
    case Paren(tgt) => flatten2(tgt)
    case other => applyExpr(flatten)(other)
  }

  val ten: Expr = Literal(10)
  val add: Expr = Ident("add")
  val addTen: Expr = Call(add, List(ten, ten))
  val multInParens: Expr = Paren(Paren(Binary("*", ten, ten)))

  val flattened: Expr = flatten2(multInParens)
}
