package ip

import ip.fileops.path._
import ip.result._
import ip.describe._
import ip.stringext._

package object ipkgops {

  def parse(input: String): Result[List[ParseError], IpkgMeta] = {

    type RL[T] = Result[List[ParseError], T]
    type NonEmptyList[T] = ::[T]

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

    def groupMultilineFields(lines: List[(String, Int)]): List[(String, Int)] =
        lines.foldLeft(List.empty[(String, Int)]){
          case (Nil, l) => List(l)
          case ((hs, hi) :: t, (ls, li)) =>
            if(ls.contains("=")) (ls, li) :: (hs, hi) :: t
            else (hs + "\n" + ls, hi) :: t
        }.reverse

    val lines: RL[NonEmptyList[(String, Int)]] =
      Result {
        input
          .split("\n").toList
          .zipWithIndex
          .filterNot(_._1.isEmpty) match {
            case _ : Nil.type => Left(List(ParseError.EmptyIpkgFile))
            case other : NonEmptyList[(String, Int)] => Right(other)
          }
      }

    lines flatMap {
      case pkg :: restOfLines =>
        val pkgcandidate = parsePackage(pkg._1)
        val pkgParsingErrors =
          if(pkgcandidate.isDefined) List.empty[ParseError]
          else List(ParseError.InvalidPackageDeclaration(pkg._1, pkg._2))
        val entryCandidates = groupMultilineFields(restOfLines)

        val (errors, entries) =
          entryCandidates
            .foldLeft((pkgParsingErrors, List.empty[(String, (String, Int))])){
              case ((errors, entries), line) =>
                parseEntry(line._1) match {
                  case None => (ParseError.InvalidKeyValuePair(line._1, line._2) :: errors, entries)
                  case Some((k, v)) => (errors, (k, (v, line._2)) :: entries)
                }
            }

        (errors, pkgcandidate) match {
          case (Nil, Some(packageName)) =>
            val lookup =
              entries.toMap

            lookup.get("modules") match {
              case None =>
                Result.failure(List(ParseError.MissingModulesSetting))
              case Some((modules, _)) =>
                lookup.get("sourcedir") match {
                  case None =>
                    Result.success(IpkgMeta(packageName, None, modules.split(",").toList.map(_.trim)))
                  case Some((sourcedir, sourcedirLine)) =>
                    Path(sourcedir) transform {
                      case Right(path) =>
                        Result.success(IpkgMeta(packageName, Some(path), modules.split(",").toList.map(_.trim)))
                      case Left(pathParsingError) =>
                        Result.failure(List(ParseError.InvalidSourcedirPath(sourcedirLine, pathParsingError)))
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

  sealed trait IpkgFileMetadata {
    protected val filePathOpt: Option[Path]
    private[ipkgops] def header =
      filePathOpt match {
        case None => "the ipkg module"
        case Some(filePath) => s"'${filePath.toString}'"
      }
    private[ipkgops] def header(lineNumber: Int) =
      filePathOpt match {
        case None => s"line ${lineNumber + 1 } of the ipkg module"
        case Some(filePath) => s"'${filePath.toString}:${lineNumber + 1 }'"
      }
    private[ipkgops] def Header(lineNumber: Int) =
      filePathOpt match {
        case None => s"Line ${lineNumber + 1 } of the ipkg module"
        case Some(filePath) => s"'${filePath.toString}:${lineNumber + 1 }'"
      }
  }
  object IpkgFileMetadata {
    def apply(filePath: Path): IpkgFileMetadata = new IpkgFileMetadata() {
      override val filePathOpt = Some(filePath)
    }

    implicit val defaultIpkgFileMetadata: IpkgFileMetadata = new IpkgFileMetadata() {
      override val filePathOpt = None
    }
  }

  sealed trait ParseError {
    def description(implicit meta: IpkgFileMetadata): String
  }

  object ParseError {

    case object EmptyIpkgFile extends ParseError {
      override def description(implicit meta: IpkgFileMetadata): String =
         "" `\n`
         "Empty IPKG" `\n`
         "" `\n`
        s"Trying to parse ${meta.header}, we have found it empty. There is nothing" ` `
         "that can be parsed out of it" `\n`
         ""
    }

    case object MissingModulesSetting extends ParseError {
      def description(implicit meta: IpkgFileMetadata): String =
         "" `\n`
         "Missing  modules  field" `\n`
         "" `\n`
        s"When parsing ${meta.header}, we have found it lacking a modules field." ` `
         "The modules field lists all the modules that should go into the final" ` `
         "package." `\n`
         "" `\n`
         "For example, given an idris package maths that has modules Maths.idr," ` `
         "Maths/NumOps.idr, Maths/BinOps.idr, and Maths/HexOps.idr, the" ` `
         "corresponding package file would be:" `\n`
         "" `\n`
         """|    package maths
            |    modules = Maths
            |            , Maths.NumOps
            |            , Maths.BinOps
            |            , Maths.HexOps
            |""".stripMargin
    }

    case class InvalidPackageDeclaration(lineContent: String, lineNumber: Int) extends ParseError {
      override def description(implicit meta: IpkgFileMetadata): String =
         "" `\n`
         "Invalid package declaration" `\n`
         "" `\n`
        s"${meta.Header(lineNumber)} contains:" `\n`
        s"    $lineContent" `\n`
         "" `\n`
         "but it's content should have the form:" `\n`
         "    package <name>" `\n`
         ""

    }

    case class InvalidKeyValuePair(lineContent: String, lineNumber: Int) extends ParseError {
      override def description(implicit meta: IpkgFileMetadata): String =
         "" `\n`
         "Invalid field" `\n`
         "" `\n`
        s"${meta.Header(lineNumber)} contains:" `\n`
        s"    $lineContent" `\n`
         "" `\n`
         "but it's content should have the form:" `\n`
         "    <key> = <value>" `\n`
         "" `\n`
         "Probably, the line is missing the '=' sign" `\n`
         ""
    }

    case class InvalidSourcedirPath(lineNumber: Int, cause: Path.FormatError) extends ParseError {
      override def description(implicit meta: IpkgFileMetadata): String =
         "" `\n`
         "Invalid sourcedir path" `\n`
         "" `\n`
        s"The sourcedir defined at ${meta.header(lineNumber)} is incorrect." `\n`
         "" `\n`
         cause.description `\n`
         ""

    }

    implicit def ParseErrorDescribe(implicit meta: IpkgFileMetadata): Describe[ParseError] =
      _.description
  }

  case class IpkgMeta(
    `package`: String,
    sourcedir: Option[Path],
    modules: List[String]
  )

}
