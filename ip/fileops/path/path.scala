package ip.fileops
package path

import ip.result._
import ip.describe._
import ip.terminate._
import java.nio.file.{
  Path  => JPath,
  Paths => JPaths,
  InvalidPathException}


sealed trait Path {

  type SELF <: Path

  def toJava: JPath

  def / (other: RelativePath): SELF

  def parent: Option[SELF]

  def / (other: String): Result[Path.ConcatenationError, SELF] = {
    Path(other).flatMap{
      case _: AbsolutePath => Result.failure(Path.NotRelative(toJava.toString, other))
      case r: RelativePath => Result.success(this / r)
    }
  }

  def last: Option[RelativePath] =
    Option(toJava.getFileName)
      .map(new RelativePath(_))
}

class RelativePath private[fileops](val toJava: JPath) extends Path{

  override type SELF = RelativePath

  override def toString: String =
    toJava.toString

  override def / (other: RelativePath): RelativePath =
    new RelativePath(toJava.resolve(other.toJava))

  override def parent: Option[RelativePath] =
    Option(toJava.getParent).map(new RelativePath(_))
}

class AbsolutePath private[fileops](val toJava: JPath) extends Path{

  override type SELF = AbsolutePath

  override def toString: String =
    toJava.toString

  override def / (other: RelativePath): AbsolutePath =
    new AbsolutePath(toJava.resolve(other.toJava))

  override def parent: Option[AbsolutePath] =
    Option(toJava.getParent).map(new AbsolutePath(_))
}

object Path {

  lazy val dot: RelativePath =
    new RelativePath(JPaths.get("."))

  lazy val current: AbsolutePath =
    new AbsolutePath(JPaths.get(".").toAbsolutePath.normalize)

  def apply(jpath: JPath): Path =
      if (jpath.isAbsolute) new AbsolutePath(jpath)
    else                    new RelativePath(jpath)

  def apply(input: String): Result[FormatError, Path] =
    Result {

      try{
        val jpath = JPaths.get(input)
        Right(apply(jpath.normalize))
      } catch {
        case ex: InvalidPathException =>
          Left(Invalid(input, ex.getReason, Option(ex.getIndex).filter(_ >= 0)))
        case t: Throwable =>
          fatal("Trying to parse a 'Path`, the JVM has thrown an undocumented exception", t)
      }
    }

//                                                                                  //
// Errors
// ________________________________________________________________________________ //

  sealed trait ConcatenationError {
    def description: String
  }
  object ConcatenationError {
    implicit val ConcatenationErrorDescribe: Describe[ConcatenationError] =
      _.description
  }

  sealed trait FormatError extends ConcatenationError{
    def description: String
  }
  object FormatError {
    implicit val FormatErrorDescribe: Describe[FormatError] =
      _.description
  }

  case class Invalid(input: String, reason: String, index: Option[Int]) extends FormatError {
    override def description: String = {
      val main =
        s"""|'$input' seems to be an invalid path.
            |Reported reason:
            |    $reason""".stripMargin
      val rest = index.map{i =>
        s"""|
            |$input
            |${" " * i}^""".stripMargin
      }

      main + rest.getOrElse("")
    }
  }

  case class NotRelative(self: String, input: String) extends ConcatenationError {
    override def description: String =
      s"""|Impossible to concatenate an absolute path to another path.
          |    Failed tried to concatenate '$input'
          |    after '$self'""".stripMargin
  }

}

