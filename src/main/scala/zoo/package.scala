package object zoo {
  final case class Fix[F[_]](unfix: F[Fix[F]])
}
