package ip.result

case class WhenHolder[T](t: T, p: T => Boolean) {

  def orFailWith[E](f: T => E): Result[E, T] =
    if   (p(t)) Result( Right(t) )
    else        Result( Left(f(t)) )

  def orFail[E](e: E): Result[E, T] =
    orFailWith(_ => e)

}
