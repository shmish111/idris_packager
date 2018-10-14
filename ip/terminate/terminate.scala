package ip

import ip.codereporting._

package object terminate {

  private def execute(label: String, message: String, exitCode: Int, throwableOpt: Option[Throwable])(implicit cr: CodeReport): Nothing = {
    println()
    println("─" * 55)
    println()
    println(label)
    println(s"    ${cr.file}:${cr.line}")
    println()
    println("Error message:")
    scala.io.Source.fromString(message).getLines.foreach(l => println(s"    $l"))
    println()
    throwableOpt foreach {throwable =>
      println("Exception causing the fatal error:")
      println(s"    ${throwable.getClass.getName}")
      println(s"    ${throwable.getMessage}")
      println()
    }
    println("Terminating the program now")
    sys.exit(exitCode)
  }

  def impossible(message: String, exitCode: Int = 10)(implicit cr: CodeReport): Nothing =
    execute("An IMPOSSIBLE bit of code has been executed at:", message, exitCode, None)

  private def fatal(message: String, exitCode: Int, throwableOpt: Option[Throwable])(implicit cr: CodeReport): Nothing =
    execute("A FATAL error has occurred at:", message, exitCode, throwableOpt)

  def fatal(message: String, exitCode: Int, throwable: Throwable)(implicit cr: CodeReport): Nothing =
    fatal(message, exitCode, Some(throwable))

  def fatal(message: String, throwable: Throwable)(implicit cr: CodeReport): Nothing =
    fatal(message, 20, Some(throwable))

  def fatal(message: String, exitCode: Int)(implicit cr: CodeReport): Nothing =
    fatal(message, exitCode, None)

  def fatal(message: String)(implicit cr: CodeReport): Nothing =
    fatal(message, 20, None)

}
