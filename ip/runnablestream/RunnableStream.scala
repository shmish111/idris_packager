package ip.runnablestream

import ip.result._

trait RunnableStream[E, T] { self =>
  def next: Either[E, Option[T]]

  def clean(): Unit = {}

  def map[U](f: T => U): RunnableStream[E, U] =
    new RunnableStream[E, U] {
      def next: Either[E, Option[U]] =
        self.next match {
          case Left(e)        => Left(e)
          case Right(None)    => Right(None)
          case Right(Some(t)) => Right(Some(f(t)))
        }
    }

  def flatMap[F, U](f: T => RunnableStream[F, U]): RunnableStream[Either[E, F], U] =
    new RunnableStream[Either[E, F], U] {
      var last: Option[RunnableStream[F, U]] = None
      def next: Either[Either[E,F], Option[U]] = {
        last match {
          case None =>
            self.next match {
              case Right(None) =>
                Right(None)
              case Right(Some(t)) =>
                last = Some(f(t))
                this.next
              case Left(e) =>
                Left(Left(e))
            }
          case Some(inner) =>
            inner.next match {
              case Right(None) =>
                last = None
                this.next
              case Right(Some(u)) =>
                Right(Some(u))
              case Left(e) =>
                Left(Right(e))
            }
        }
      }
    }

  def mapFalible[F, U](f: T => Result[F, U]): RunnableStream[Either[E, F], U] =
    new RunnableStream[Either[E, F], U] {
      def next: Either[Either[E, F], Option[U]] =
        self.next match {
          case Right(Some(t)) =>
            f(t).run match {
              case Right(u) => Right(Some(u))
              case Left(e) => Left(Right(e))
            }
          case Right(None) =>
            Right(None)
          case Left(e) =>
            Left(Left(e))
        }
    }

  object run {
    def foreach(f: T => Unit): Either[E, Unit] = {
      next match {
        case Left(e) => clean(); Left(e)
        case Right(None) => clean(); Right(())
        case Right(Some(t)) =>
          f(t)
          foreach(f)
      }
    }

    def apply(): Either[E, Unit] =
      foreach(_ => ())

    def asResult: Result[E, Unit] = {
      def generate: Either[E, Unit] =
        next match {
          case Left(e) => Left(e)
          case Right(None) => Right(())
          case Right(_) => generate
        }
      Result.create(generate, clean _)
    }
  }
}

