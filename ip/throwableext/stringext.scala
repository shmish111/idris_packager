package ip

package object throwableext {

  implicit class ThrowableExt[T <: Throwable](val t: T) extends AnyVal {
    def description(prefix: String): String =
      s"""|$prefix
          |
          |Caused by this exception:
          |    ${t.getClass.getName}
          |    ${t.getMessage}"
          |""".stripMargin
  }

}

