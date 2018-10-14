package ip


//import ip.result.TupleSequenceExtensions

package object result extends TupleSequenceExtensions {
  import scala.collection.generic.CanBuildFrom

  type RUnit[+E] = Result[E, Unit]

  private implicit class TraversableExtensions[C[T] <: Traversable[T], A](val c: C[A]) extends AnyVal {
    def doMap[B](f: A => B)(implicit cb: CanBuildFrom[C[A], B, C[B]]): C[B] = {
      val ab = cb(c)
      c foreach {e =>
        ab += f(e)
      }

      ab.result
    }
  }

  private implicit class CollectionOfEitherOps[T, E, C[A] <: Traversable[A]](val collection: C[Either[E, T]]) extends AnyVal {
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

  implicit class CollectionOfResultsOps[T, E, C[A] <: Traversable[A]](val collection: C[Result[E, T]]) extends AnyVal {
    def sequence(implicit
      cbe: CanBuildFrom[C[Either[E, T]], E, C[E]],
      cbt: CanBuildFrom[C[Either[E, T]], T, C[T]],
      cbr: CanBuildFrom[C[Result[E, T]], Either[E, T], C[Either[E, T]]]): Result[C[E], C[T]] = new Result[C[E], C[T]]{

        private[result] def gen: Either[C[E],C[T]] =
          collection.doMap(_.gen).sequence

        private[result] def clean: Unit =
          collection.foreach(_.clean)

    }

    def failFastSequence(implicit
      cbf: CanBuildFrom[C[Result[E, T]], T, C[T]]): Result[E, C[T]] = new Result[E, C[T]]{

      lazy val (toFree: List[Result[E, T]], generated: Either[E, C[T]]) = {
        val tf = List.newBuilder[Result[E, T]]
        val g = cbf(collection)
        val i = collection.toIterator
        var error: Option[E] = None

        while (error.isEmpty && i.hasNext) {
          val r = i.next
          tf += r
          r.gen match {
            case Right(e) =>
              g += e
            case Left(e) =>
              error = Some(e)
          }
        }

        error match {
          case Some(error) =>
            (tf.result, Left(error))
          case None =>
            (tf.result, Right(g.result))
        }
      }

      private[result] def gen: Either[E, C[T]] = generated
      private[result] def clean: Unit = toFree.foreach(_.clean)

    }
  }

  implicit class AnyOpsForResult[T](val t: T) extends AnyVal {
    def when(p: T => Boolean): WhenHolder[T] =
      WhenHolder(t, p)
  }

  implicit class EitherOpsForResult[E, T](val e: Either[E, T]) extends AnyVal {
    def asResult: Result[E, T] =
      Result(e)
  }

  implicit class OptionOpsForResult[T](val o: Option[T]) extends AnyVal {
    def toSuccess[E](error: E): Result[E, T] =
      o match {
        case Some(t) => Result.success(t)
        case None => Result.failure(error)
      }
  }

}
