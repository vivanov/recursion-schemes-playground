package playground

import cats.Defer

object Fixpoint {
  def parens(n: Int): String =  
   if(n == 0) s"$n" 
   else {
     val s = parens(n -1)
     s"($s)"
   }

   // This is a call for Scalaz original implementation of fix for Function
   // val parensF = fix[Int => String](parens => n => if(n == 0) s"$n" else s"(${parens.apply(n - 1)})")

   // Cats implementation of fix uses Defer typeclass and there is an instance for Function1
   type FuncInt[A] = Function1[Int, A]
   val parensF = Defer[FuncInt].fix[String](parens => n => if(n == 0) s"$n" else s"(${parens.apply(n - 1)})")

}
