package ip.result

import ip.terminate._

trait TupleSequenceExtensions {
  implicit class TupleSequenceExtension2[A, B, E, F](tup: (Result[E, A], Result[F, B])) {
    def combine: Result[Nothing, (Either[E, A], Either[F, B])] =
      tup._1 combine tup._2
  }
  implicit class SameErrorTupleSequenceExtension2[A, B, E](tup: (Result[E, A], Result[E, B])) {
    def sequence: Result[List[E], (A, B)] =
      tup.combine flatMap {
        case (Right(a), Right(b)) => Result.success((a, b))
        case (Left(ea), Right(_)) => Result.failure(List(ea))
        case (Right(_), Left(eb)) => Result.failure(List(eb))
        case (Left(ea), Left(eb)) => Result.failure(List(ea, eb))
      }
  }
  implicit class TupleSequenceExtension3[A, B, C, E, F, G](tup: (Result[E, A], Result[F, B], Result[G, C])) {
    def combine: Result[Nothing, (Either[E, A], Either[F, B], Either[G, C])] =
      (tup._1 combine tup._2 combine tup._3).map {
        case (Right((a, b)), c) => (a, b, c)
        case (Left(_ /* : Nothing */), _) =>
          impossible(
            """|The type of
               |  tup._1 combine tup._2 combine tup._3
               |is
               |  Result[Nothing,(Either[Nothing,(Either[E,A], Either[F,B])], Either[G,C])]
               |which means that in the Left of a successful result, there is Nothing.
               |Which is impossible""".stripMargin)
      }
  }
  implicit class SameErrorTupleSequenceExtension3[A, B, C, E](tup: (Result[E, A], Result[E, B], Result[E, C])) {
    def sequence: Result[List[E], (A, B, C)] =
      tup.combine flatMap {
        case (Right(a), Right(b), Right(c)) => Result.success((a, b, c))

        case (Right(_), Right(_), Left(ec)) => Result.failure(List(ec))

        case (Left(ea), Right(_), Right(_)) => Result.failure(List(ea))
        case (Right(_), Left(eb), Right(_)) => Result.failure(List(eb))
        case (Left(ea), Left(eb), Right(_)) => Result.failure(List(ea, eb))

        case (Left(ea), Right(_), Left(ec)) => Result.failure(List(ea, ec))
        case (Right(_), Left(eb), Left(ec)) => Result.failure(List(eb, ec))
        case (Left(ea), Left(eb), Left(ec)) => Result.failure(List(ea, eb, ec))

      }
  }
}

