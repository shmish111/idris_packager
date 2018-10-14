package ip

import annotation.implicitNotFound

package describe {

  @implicitNotFound(msg = "Cannot find Describe type class for ${T}")
  trait Describe[T] {
    def description(t: T): String
  }

}

package object describe {

  implicit class DescribeOps[T](val t: T) extends AnyVal {
    def description(implicit d: Describe[T]): String =
      d.description(t)
  }

}
