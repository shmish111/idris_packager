package ip

package resource {

  import scala.util.{Failure, Success, Try}

  trait Resource[+E, +A] {self =>

    private[resource] def gen(): Either[E, A]
    private[resource] def clean(): Unit

    def run(): Either[E, A] = {
      try {
        gen()
      }
      finally {
        clean()
      }
    }

    def withCleanup(func: () => Any): Resource[E, A] =
      new Resource[E, A] {
        override private[resource] def gen(): Either[E, A] = self.gen

        override private[resource] def clean(): Unit = {
          try func() finally self.clean
          ()
        }
      }


    def map[B](func: A => B): Resource[E, B] =
      new Resource[E, B] {
        override private[resource] def gen(): Either[E, B] = self.gen.map(func)

        override private[resource] def clean(): Unit = self.clean
      }

    def flatMap[F >: E, B](func: A => Resource[F, B]): Resource[F, B] =
      new Resource[F, B] {
        lazy val ttb: Try[Resource[F, B]] =
          Try{
            self.gen match {
              case Right(a) => func(a)
              case Left(e) => Resource.create( Left(e), () => () )
            }
          }

        override private[resource] def gen(): Either[F, B] = ttb match {
          case Success(tb) =>
            tb.gen
          case Failure(f) =>
            throw f
        }

        override private[resource] def clean(): Unit = ttb match {
          case Success(tb) =>
            try tb.clean finally self.clean
            ()
          case Failure(_) =>
            self.clean
        }
      }

    def mapError[F](f: E => F): Resource[F, A] =
      new Resource[F, A] {
        override private[resource] def gen(): Either[F, A] = self.gen.left.map(f)

        override private[resource] def clean(): Unit = self.clean
      }

  }

  object Resource {

    def create[E, A](generate: =>  Either[E, A], cleanup: () => Any): Resource[E, A] =
      new Resource[E, A] {
        override private[resource] def gen(): Either[E, A] = generate

        override private[resource] def clean(): Unit = {cleanup(); ()}
      }

    def apply[E, A](func: => Either[E, A]): Resource[E, A] =
      Resource.create[E, A]  ( func, () => ())

    def cleanup(func: () => Any): Resource[Nothing, Nothing] =
      Resource.create[Nothing, Nothing]  ( throw new Exception("Gen function missing"), func)

    def success[A](func: => A): Resource[Nothing, A] =
      Resource.create[Nothing, A] ( Right(func), () => ())

    def failure[E](func: => E): Resource[E, Nothing] =
      Resource.create[E, Nothing] ( Left(func), () => ())

  }

  case class WhenHolder[T](t: T, p: T => Boolean) {

    def orFailWith[E](f: T => E): Resource[E, T] =
      if   (p(t)) Resource( Right(t) )
      else        Resource( Left(f(t)) )

    def orFail[E](e: E): Resource[E, T] =
      orFailWith(_ => e)

  }

}

package object resource {
  import scala.collection.generic.CanBuildFrom

  type RUnit[+E] = Resource[E, Unit]

  private implicit class TraversableExtensions[C[T] <: Traversable[T], A](val c: C[A]) extends AnyVal {
    def doMap[B](f: A => B)(implicit cb: CanBuildFrom[C[A], B, C[B]]): C[B] = {
      val ab = cb(c)
      c foreach {e =>
        ab += f(e)
      }

      ab.result
    }
  }

  implicit class CollectionOfResourcesOps[T, E, C[A] <: Traversable[A]](val collection: C[Resource[E, T]]) extends AnyVal {
    def sequence(implicit
      cbe: CanBuildFrom[C[Either[E, T]], E, C[E]],
      cbt: CanBuildFrom[C[Either[E, T]], T, C[T]],
      cbr: CanBuildFrom[C[Resource[E, T]], Either[E, T], C[Either[E, T]]]): Resource[C[E], C[T]] = new Resource[C[E], C[T]]{

        private lazy val eithers: Either[C[E], C[T]] = {
          import ip.eitherops._
          collection.doMap(_.gen).sequence
        }

        private[resource] def gen: Either[C[E],C[T]] = eithers
        private[resource] def clean: Unit = {
          collection.foreach(_.clean)
        }

    }

    def failFastSequence(implicit
      cbf: CanBuildFrom[C[Resource[E, T]], T, C[T]]): Resource[E, C[T]] = new Resource[E, C[T]]{

      lazy val (toFree: List[Resource[E, T]], generated: Either[E, C[T]]) = {
        val tf = List.newBuilder[Resource[E, T]]
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

      private[resource] def gen: Either[E, C[T]] = generated
      private[resource] def clean: Unit = toFree.foreach(_.clean)

    }
  }

  implicit class AnyOpsForResource[T](val t: T) extends AnyVal {
    def when(p: T => Boolean): WhenHolder[T] =
      WhenHolder(t, p)
  }

  implicit class EitherOpsForResource[E, T](val e: Either[E, T]) extends AnyVal {
    def asResource: Resource[E, T] =
      Resource(e)
  }

}


