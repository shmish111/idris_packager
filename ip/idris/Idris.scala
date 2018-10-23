package ip.idris

import ip.fileops.path.AbsolutePath
import ip.logger.Logger
import ip.result._
import ip.describe._

sealed trait Idris {
  type Self <: Idris
  val wd: AbsolutePath
  def apply(args: String*)(implicit logger: Logger): Result[NonZeroExit, Unit]
  def from(newWd: AbsolutePath): Self
}

object Idris {

  case class Plain(idrisPath: AbsolutePath, override val wd: AbsolutePath) extends Idris {
    override type Self = Plain
    override def apply(args: String*)(implicit logger: Logger): Result[NonZeroExit, Unit] = {
      Result{
        import scala.sys.process._
        val params = Seq(idrisPath.toString) ++ args
        logger.trace(
          s"""|Going to execute idris, with command:
              |    ${params.mkString(" ")}""".stripMargin)
        val r =
          Process(
            params,
            Option(wd.toJava.toFile)).!
        logger.trace("Idris execution completed")
        if (r == 0) Right(())
        else Left(NonZeroExit(r))
      }
    }

    override def from(newWd: AbsolutePath): Plain =
      this.copy(wd = newWd)
  }

}

final case class NonZeroExit(exitCode: Int)
object NonZeroExit {
  implicit val NonZeroExitDescribe: Describe[NonZeroExit] =
    (n: NonZeroExit) =>
      s"""Idris execution has faild, with exit code ${n.exitCode}"""
}
