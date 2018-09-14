package ip.fileops
package path

import java.nio.file.{
  Path  => JPath,
  Paths => JPaths,
  InvalidPathException}


sealed trait Path {

  type SELF <: Path

  def toJava: JPath

  def / (other: RelativePath): SELF

  def parent: Option[SELF]

  def / (other: String): Either[Path.PathConcatenationError, SELF] = {
    Path(other).flatMap{
      case _: AbsolutePath => Left(Path.NotRelative(other))
      case r: RelativePath => Right(this / r)
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

  sealed trait PathConcatenationError

  sealed trait PathFormatError extends PathConcatenationError
  case class Invalid(input: String, reason: String, index: Option[Int]) extends PathFormatError

  case class NotRelative(input: String) extends PathConcatenationError

  def apply(jpath: JPath): Path =
      if (jpath.isAbsolute) new AbsolutePath(jpath)
    else                    new RelativePath(jpath)

  def apply(input: String): Either[PathFormatError, Path] = {
    try {  Right(apply(JPaths.get(input).normalize))  }
    catch {
      case ex: InvalidPathException =>
        Left(Invalid(input, ex.getReason, Option(ex.getIndex).filter(_ >= 0)))
    }
  }
}

