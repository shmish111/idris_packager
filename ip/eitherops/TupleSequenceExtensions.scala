package ip.eitherops

trait TupleSequenceExtensions {
  implicit class TupleSequenceExtension2[A, B, E](tup: (Either[E, A], Either[E, B])) {
    def sequence: Either[List[E], (A, B)] =
      tup match {
        case (Right(a), Right(b)) => Right((a, b))
        case (Left(ea), Right(_)) => Left(List(ea))
        case (Right(_), Left(eb)) => Left(List(eb))
        case (Left(ea), Left(eb)) => Left(List(ea, eb))
      }
  }
}
