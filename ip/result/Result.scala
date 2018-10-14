package ip.result

import scala.util.{Failure, Success, Try}

trait Result[+E, +A] {self =>

  private[result] def gen(): Either[E, A]
  private[result] def clean(): Unit

  def run(): Either[E, A] = {
    try {
      gen()
    }
    finally {
      clean()
    }
  }

  def withCleanup(func: () => Any): Result[E, A] =
    new Result[E, A] {
      override private[result] def gen(): Either[E, A] = self.gen

      override private[result] def clean(): Unit = {
        try func() finally self.clean
        ()
      }
    }


  def map[B](func: A => B): Result[E, B] =
    new Result[E, B] {
      override private[result] def gen(): Either[E, B] = self.gen.map(func)

      override private[result] def clean(): Unit = self.clean
    }

  def flatMap[F >: E, B](func: A => Result[F, B]): Result[F, B] =
    transform{
      case Right(a) => func(a)
      case Left(e) =>  Result.create( Left(e), () => () )
    }

  def mapError[F](f: E => F): Result[F, A] =
    new Result[F, A] {
      override private[result] def gen(): Either[F, A] = self.gen.left.map(f)

      override private[result] def clean(): Unit = self.clean
    }

  def flatMapError[F, B >: A](func: E => Result[F, B]): Result[F, B] =
    transform{
      case Right(a) => Result.create( Right(a), () => () )
      case Left(e) => func(e)
    }

  def transform[F, B](func: Either[E, A] => Result[F, B]): Result[F, B] =
    new Result[F, B] {
      lazy val ttb: Try[Result[F, B]] =
        Try{
          func(self.gen)
        }

      override private[result] def gen(): Either[F, B] = ttb match {
        case Success(tb) =>
          tb.gen
        case Failure(f) =>
          throw f
      }

      override private[result] def clean(): Unit = ttb match {
        case Success(tb) =>
          try tb.clean finally self.clean
          ()
        case Failure(_) =>
          self.clean
      }
    }

  def combine[F, B](that: Result[F, B]): Result[Nothing, (Either[E, A], Either[F, B])] =
    new Result[Nothing, (Either[E, A], Either[F, B])] {
      lazy val teaeb = Try((self.gen, that.gen))

      override private[result] def gen(): Either[Nothing, (Either[E,A], Either[F,B])] = teaeb match {
        case Success(eaeb) =>
          Right(eaeb)
        case Failure(f) =>
          throw f
      }

      override private[result] def clean(): Unit =
          try self.clean finally that.clean
    }
}

object Result {

  def create[E, A](generate: =>  Either[E, A], cleanup: () => Any): Result[E, A] =
    new Result[E, A] {
      override private[result] def gen(): Either[E, A] = generate

      override private[result] def clean(): Unit = {cleanup(); ()}
    }

  def apply[E, A](func: => Either[E, A]): Result[E, A] =
    Result.create[E, A]  ( func, () => ())

  def success[A](func: => A): Result[Nothing, A] =
    Result.create[Nothing, A] ( Right(func), () => ())

  def failure[E](func: => E): Result[E, Nothing] =
    Result.create[E, Nothing] ( Left(func), () => ())

}

