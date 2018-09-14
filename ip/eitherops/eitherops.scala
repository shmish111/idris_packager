package ip

import scala.collection.generic.CanBuildFrom

package object eitherops extends TupleSequenceExtensions {

  type EUnit[+E] = Either[E, Unit]
  val EOK: EUnit[Nothing] = Right(())

  implicit class CollectionOfResultsOps[T, E, C[A] <: Traversable[A]](val collection: C[Either[E, T]]) extends AnyVal {
    def sequence(implicit
      cbe: CanBuildFrom[C[Either[E, T]], E, C[E]],
      cbt: CanBuildFrom[C[Either[E, T]], T, C[T]]): Either[C[E], C[T]] = {

        val eb = cbe(collection)
        val tb = cbt(collection)

        collection foreach {
          case Right(r) => tb += r
          case Left(l)  => eb += l
        }

        val re = eb.result
        val rt = tb.result

        if (re.isEmpty) Right(rt)
        else Left(re)
    }
  }

  implicit class AnyOpsForEither[T](val t: T) extends AnyVal {
    def when(p: T => Boolean): WhenHolder[T] =
      WhenHolder(t, p)
  }


  implicit class EitherResultOps[T, E](e: Either[E, T]) {
    def mapError[F](f: E => F): Either[F, T] =
      e.left.map(f)
    def flatMapError[F, U >: T](f: E => Either[F, U]): Either[F, U] =
      e.left.flatMap(f)
  }

}

package eitherops {

  case class WhenHolder[T](t: T, p: T => Boolean) {

    def orFailWith[E](f: T => E): Either[E, T] =
      if   (p(t)) Right(t)
      else        Left(f(t))

    def orFail[E](e: E): Either[E, T] =
      orFailWith(_ => e)

  }

}
