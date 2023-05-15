package zoo

trait Functor[F[_]] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]
}

object Functor {
  def apply[F[_]](implicit instance: Functor[F]) = instance
  object Instances {
    implicit val listFunctor = new Functor[List] {
      override def map[A, B](f: A => B)(la: List[A]): List[B] = la.map(f)
    }

    implicit val optionFunctor = new Functor[Option] {
      override def map[A, B](f: A => B)(maybeA: Option[A]): Option[B] = maybeA match {
        case None => None
        case Some(a) => Some(f(a))
      }
    }

    type EitherError[A] = Either[Exception, A]
    implicit val eitherErrorFunctor = new Functor[EitherError] {
      override def map[A, B](f: A => B)(errorOrA: EitherError[A]): EitherError[B] = errorOrA match {
        case Left(error) => Left(error)
        case Right(a) => Right(f(a))
      }
    }
  }

  object Syntax {
    implicit class FunctorSyntax[F[_], A](fa: F[A])(implicit F: Functor[F]) {
      def map[B](f: A => B): F[B] = F.map(f)(fa)
    }
  }
}
