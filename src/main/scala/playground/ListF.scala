package playground

import zoo.Fix
import zoo.Functor
import zoo.Schemes._

sealed trait ListF[A, B]

object ListF {
  final case class NilF[A, B]() extends ListF[A, B]
  final case class ConsF[A, B](a: A, b: B) extends ListF[A, B]

  def nil[A, B]: ListF[A, B] = NilF[A, B]()
  def cons[A, B](a: A, b: B): ListF[A, B] = ConsF[A, B](a, b)

  type ListFString[B] = ListF[String, B]

  implicit val listStringFunctor = new Functor[ListFString] {
    override def map[T, U](f: T => U)(list: ListFString[T]): ListFString[U] = list match {
      case NilF() => NilF()
      case ConsF(s, t) => ConsF(s, f(t))
    }
  }

  def sumSize: Algebra[ListFString, Int] = {
    case NilF() => 0
    case ConsF(a, b) => a.size + b
  }

  val list = List.unfold(0){
    case 0 => Some("abc", 1)
    case 1 => Some("xy", 2)
    case 2 => Some("!", 3)
    case 3 => None
  }

  val nilStr: Fix[ListFString] = Fix(nil)
  def consStr(a: String, b: Fix[ListFString]): Fix[ListFString] = Fix(cons(a, b))

  val stringsF: Fix[ListFString] = consStr("abc", consStr("xy", consStr("!", nilStr)))

  val sizedF = cata(sumSize)(stringsF)
}


