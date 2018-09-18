package ip.tupleops

trait TupleMapExtensions {
  implicit class TupleMapExtension2[A](tup: (A, A)) {
    def map[B](f: A => B): (B, B) =
      tup match {
        case (a, b) => (f(a), f(b))
      }
  }

  implicit class TupleMapExtension3[A](tup: (A, A, A)) {
    def map[B](f: A => B): (B, B, B) =
      tup match {
        case (a, b, c) => (f(a), f(b), f(c))
      }
  }
}
