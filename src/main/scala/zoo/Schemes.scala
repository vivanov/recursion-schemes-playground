package zoo

import Functor.Syntax._

object Schemes {
  def topDown[F[_]: Functor](f: Fix[F] => Fix[F])(term: Fix[F]): Fix[F] = {
    val applied: Fix[F] = f(term)
    val unpacked: F[Fix[F]] = applied.unfix
    val recurseWith: Fix[F] => Fix[F] = topDown(f)
    val recursed: F[Fix[F]] = unpacked.map(recurseWith)
    val repacked: Fix[F] = Fix(recursed)
    repacked
  }

  def mystery[F[_]: Functor, A](f: F[A] => A)(term: Fix[F]): A = {
    val unpacked: F[Fix[F]] = term.unfix
    val recurseWith: Fix[F] => A = mystery(f) 
    val recursed: F[A] = unpacked.map(recurseWith)
    val applied: A = f(recursed)
    applied 
  }

  type Algebra[F[_], A] = F[A] => A

  def cata[F[_]: Functor, A](f: Algebra[F, A])(term: Fix[F]): A = {
    val unpacked: F[Fix[F]] = term.unfix
    val recurseWith: Fix[F] => A = cata(f) 
    val recursed: F[A] = unpacked.map(recurseWith)
    val applied: A = f(recursed)
    applied 
  }

  def bottomUpInTermsOfCata[F[_]: Functor, A](f: Fix[F] => Fix[F])(term: Fix[F]): Fix[F] = {
    val g: F[Fix[F]] => Fix[F] = Fix.apply[F] _ andThen f
    cata[F, Fix[F]](g)(term)
  }

  def what[F[_]: Functor, A](f: A => F[A])(a: A): Fix[F] = {
    val applied: F[A] = f(a)
    val recurseWith: A => Fix[F] = what(f)
    val recursed: F[Fix[F]] = applied.map(recurseWith)
    val repacked: Fix[F] = Fix(recursed)
    repacked
  }

  type Coalgebra[F[_], A] = A => F[A]

  def ana[F[_]: Functor, A](f: Coalgebra[F, A])(a: A): Fix[F] = {
    val applied: F[A] = f(a)
    val recurseWith: A => Fix[F] = ana(f)
    val recursed: F[Fix[F]] = applied.map(recurseWith)
    val repacked: Fix[F] = Fix(recursed)
    repacked
  }

  def topDownInTermsOfAna[F[_]: Functor](f: Fix[F] => Fix[F])(term: Fix[F]): Fix[F] = {
    val g: Fix[F] => F[Fix[F]] = ((_: Fix[F]).unfix) compose f
    ana[F, Fix[F]](g)(term)
  }

  def hyloNaive[F[_]: Functor, A, B](f: Algebra[F, B])(g: Coalgebra[F, A]): A => B =
    ana(g) _ andThen cata(f)

  def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    a => f(g(a).map(hylo(f)(g)))
}
