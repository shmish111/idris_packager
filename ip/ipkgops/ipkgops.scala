package ip

import ip.fileops.path._
import ip.result._

package object ipkgops {

  def parse(input: String): Result[List[ParseError], IpkgMeta] = {

    def parsePackage(line: String): Option[String] = {
      val base = line.trim
      val rq = raw"""package\s+"(.*)"""".r
      val rnq = raw"package\s+(.*)".r
      base match {
        case rq(packageName) => Some(packageName)
        case rnq(packageName) => Some(packageName)
        case _ => None
      }
    }

    def parseEntry(entryCandidate: String): Option[(String, String)] =
      entryCandidate.split("=", 2).toList match {
        case k :: v :: Nil => Some(k.trim -> v.trim)
        case _ => None
      }

    val lines =
      input
        .split("\n")
        .toList
        .map(_.trim)
        .zipWithIndex
        .filterNot(_._1.isEmpty)

    lines match {
      case Nil => Result.failure(List(ParseError.EmptyIpkgFile))
      case pkg :: entryCandidates =>
        val pkgcandidate = parsePackage(pkg._1)
        val pkgParsingErrors =
          if(pkgcandidate.isDefined) List.empty[ParseError]
          else List(ParseError.InvalidPackageDeclaration(pkg._2))

        val (errors, entries) =
          entryCandidates
            .foldLeft((pkgParsingErrors, List.empty[(String, String)])){
              case ((errors, entries), line) =>
                parseEntry(line._1) match {
                  case None => (ParseError.InvalidKeyValuePair(line._2) :: errors, entries)
                  case Some(entry) => (errors, entry :: entries)
                }
            }

        (errors, pkgcandidate) match {
          case (Nil, Some(packageName)) =>
            val lookup =
              entries.toMap

            lookup.get("modules") match {
              case None =>
                Result.failure(List(ParseError.MissingModulesSetting))
              case Some(modules) =>
                lookup.get("sourcedir") match {
                  case None =>
                    Result.success(IpkgMeta(packageName, None, modules.split(",").toList.map(_.trim)))
                  case Some(sourcedir) =>
                    Path(sourcedir) transform {
                      case Right(path) =>
                        Result.success(IpkgMeta(packageName, Some(path), modules.split(",").toList.map(_.trim)))
                      case Left(pathParsingError) =>
                        val (_, line) =
                          lines
                            .filter(_._1.matches("^\\s*sourcedir\\s*="))
                            .reverse
                            .head
                        Result.failure(List(ParseError.InvalidSourcedirPath(line, pathParsingError)))
                    }
                }
            }
          case (_, _) =>
            Result.failure(errors)
        }
    }
  }
}

package ipkgops {

  sealed trait ParseError
  object ParseError {
    case object EmptyIpkgFile                                            extends ParseError
    case class  InvalidPackageDeclaration(line: Int)                     extends ParseError
    case class  InvalidKeyValuePair(line: Int)                           extends ParseError
    case object MissingModulesSetting                                    extends ParseError
    case class  InvalidSourcedirPath(line: Int, cause: Path.FormatError) extends ParseError
  }

  case class IpkgMeta(
    `package`: String,
    sourcedir: Option[Path],
    modules: List[String]
  )

}
