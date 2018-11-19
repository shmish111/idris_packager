package ip.idris

import ip.fileops.path.AbsolutePath
import ip.logger.Logger
import ip.result._
import ip.describe._

sealed trait Idris {
  type Self <: Idris
  val wd: AbsolutePath
  def apply(args: String*)(implicit logger: Logger): Result[IdrisExecutionError, Unit]
  def from(newWd: AbsolutePath): Self
}

object Idris {

  case class Plain(idrisPath: AbsolutePath, override val wd: AbsolutePath) extends Idris {
    override type Self = Plain
    override def apply(args: String*)(implicit logger: Logger): Result[IdrisExecutionError, Unit] = {
      Result{
        import scala.sys.process._
        val params = Seq(idrisPath.toString) ++ args
        logger.trace(
          s"""|Going to execute idris, with command:
              |    ${params.mkString(" ")}""".stripMargin)
        val r =
          Process(
            params,
            Option(wd.toJava.toFile)).run(true).exitValue
        logger.trace("Idris execution completed")
        if (r == 0) Right(())
        else Left(IdrisExecutionError.NonZeroExit(r))
      }
    }

    override def from(newWd: AbsolutePath): Plain =
      this.copy(wd = newWd)
  }

  case class WithModules(plain: Plain, modules: List[AbsolutePath]) extends Idris {
    override type Self = WithModules
    override val wd: AbsolutePath = plain.wd
    override def from(newWd: AbsolutePath): Self =
      copy(plain = plain.from(newWd))

    private var _installedModules: Result[IdrisExecutionError.ModuleInstallError, List[AbsolutePath]] = null
    private def installedModules(implicit logger: Logger): Result[IdrisExecutionError.ModuleInstallError, List[AbsolutePath]] = {
      if(_installedModules == null)
        _installedModules = modules.map(install.install).sequence.mapError(es => IdrisExecutionError.ModuleInstallError(es.mkString("\n")))

      _installedModules
    }

    override def apply(args: String*)(implicit logger: Logger): Result[IdrisExecutionError, Unit] =
      installedModules.flatMap{modules =>
        val allArgs = modules.flatMap(m => List("-i", m.toString)) ++ args
        plain(allArgs :_*)
      }
  }
  object WithModules {
    def apply(idrisPath: AbsolutePath, wd: AbsolutePath, modules: List[AbsolutePath]): WithModules = {
      val plain = Idris.Plain(idrisPath, wd)
      new WithModules(plain, modules)
    }
  }

}

sealed trait IdrisExecutionError {
  def description: String
}

object IdrisExecutionError {

  case class NonZeroExit(exitCode: Int) extends IdrisExecutionError {
    override def description: String =
      s"""Idris execution has faild, with exit code $exitCode"""
  }

  case class ModuleInstallError(override val description: String) extends IdrisExecutionError

  implicit val IdrisExecutionErrorDescribe: Describe[IdrisExecutionError] =
    _.description
}
