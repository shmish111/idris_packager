package ip.logger

import annotation.implicitNotFound

@implicitNotFound(msg = """Cannot find a Logger in scope.
Please add a Logger implicit parameter to the wrapping function or,
import ip.logger.Logger.console.<log_level>
the default Logger""")
trait Logger {
  def trace(message: String): Unit
  def debug(message: String): Unit
  def info(message: String): Unit
  def warn(message: String): Unit
  def error(message: String): Unit
}

object Logger {

  object Console {

    private def out(message: String, first: String, rest: String): Unit = {
      val txt = if(message endsWith "\n") message + "\n" else message
      scala.io.Source.fromString(txt).getLines.toList match {
        case h :: t =>
          println(s"$first$h")
          t.foreach(l => println(s"$rest$l"))
        case _ =>
          println(s"IMPOSSIBLE: splitting the string '$message', should never result into an empty string")
          sys.exit(1)
      }
    }

    private def out(message: String, first: String): Unit =
      out(message, first, " " * first.length)

    implicit object trace extends Logger {
      def trace (message: String): Unit = out(message, ". ", "  ")
      def debug (message: String): Unit = out(message, "> ", "  ")
      def info  (message: String): Unit = out(message, "", "  ")
      def warn  (message: String): Unit = out(message, "WARNING: ")
      def error (message: String): Unit = out(message, "ERROR: ")
    }

    implicit object debug extends Logger {
      def trace (message: String): Unit = ()
      def debug (message: String): Unit = out(message, "> ", "  ")
      def info  (message: String): Unit = out(message, "", "  ")
      def warn  (message: String): Unit = out(message, "WARNING: ")
      def error (message: String): Unit = out(message, "ERROR: ")
    }

    implicit object info extends Logger {
      def trace (message: String): Unit = ()
      def debug (message: String): Unit = ()
      def info  (message: String): Unit = out(message, "", "  ")
      def warn  (message: String): Unit = out(message, "WARNING: ")
      def error (message: String): Unit = out(message, "ERROR: ")
    }

    implicit object warn extends Logger {
      def trace (message: String): Unit = ()
      def debug (message: String): Unit = ()
      def info  (message: String): Unit = ()
      def warn  (message: String): Unit = out(message, "WARNING: ")
      def error (message: String): Unit = out(message, "ERROR: ")
    }

    implicit object error extends Logger {
      def trace (message: String): Unit = ()
      def debug (message: String): Unit = ()
      def info  (message: String): Unit = ()
      def warn  (message: String): Unit = ()
      def error (message: String): Unit = out(message, "ERROR: ")
    }

  }

}
