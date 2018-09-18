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
  implicit class TupleSequenceExtension3[A, B, C, E](tup: (Either[E, A], Either[E, B], Either[E, C])) {
    def sequence: Either[List[E], (A, B, C)] =
      tup match {
        case (Right(a), Right(b), Right(c)) => Right((a, b, c))

        case (Right(_), Right(_), Left(ec)) => Left(List(ec))

        case (Left(ea), Right(_), Right(_)) => Left(List(ea))
        case (Right(_), Left(eb), Right(_)) => Left(List(eb))
        case (Left(ea), Left(eb), Right(_)) => Left(List(ea, eb))

        case (Left(ea), Right(_), Left(ec)) => Left(List(ea, ec))
        case (Right(_), Left(eb), Left(ec)) => Left(List(eb, ec))
        case (Left(ea), Left(eb), Left(ec)) => Left(List(ea, eb, ec))

      }
  }
}
