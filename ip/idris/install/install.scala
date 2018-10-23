package ip.idris

import ip.fileops.path._
import ip.fileops._
import ip.resources._
import ip.ipkgops._
import ip.logger.Logger
import ip.result._
import ip.describe.Describe
import ip.zipops._
import ip.stringext._

package object install {

  def install(ipzPath: AbsolutePath)(implicit logger: Logger): Result[String, AbsolutePath] =
    for {
      extractionPath <- temporaryDirectory.mapError(_.toString)
      _               = logger.trace(s"Module '$ipzPath' is going to be extracted into '$extractionPath'")
      _              <- unzip(ipzPath, extractionPath).mapError(_.toString)
      ipkgPath       <- findModuleIpkgPath(ipzPath, extractionPath).mapError(_.description)
      _               = logger.trace(s"'$ipkgPath' is ipkg file of '$ipzPath'")
      content        <- readUTF8(ipkgPath)
                            .mapError(error => s"The content of the file could not be read because: $error")
      _               = logger.trace(s"The content of '$ipkgPath' is:\n\n${content.tab}")
      ipkgMeta       <- parse(content)
                            .mapError(errors => s"The content could not be parsed due to:\n${errors.mkString("\n")}")
      sourcedir      <- ipkgMeta
                            .sourcedir
                            .getOrElse(Path.dot) match {
                               case r: RelativePath => Result.success(r)
                               case a: AbsolutePath => Result.failure(s"Only relative sourcedirs are accepted, but the module has '$a' configured")
                             }
    } yield {
      val r =
        extractionPath / sourcedir
      logger.debug(s"Module '$ipzPath' successfuly installed into '$r'")
      r
    }

  private def findModuleIpkgPath(module: AbsolutePath, extractionPath: AbsolutePath): Result[InstallError.IpkgFinding, AbsolutePath] =
    extractionPath
      .whenDir{dir =>
         dir.find(raw".*\.ipkg") transform {
           case Right(target :: Nil) =>
             Result.success(extractionPath / target)
           case Right(Nil) =>
             Result.failure(InstallError.IpkgFinding.NoCandidates(module, extractionPath))
           case Right(candidates) =>
             Result.failure(InstallError.IpkgFinding.MultipleCandidates(module, extractionPath, candidates))
           case Left(cause) =>
             Result.failure(InstallError.IpkgFinding.FindInvocationError(module, extractionPath, cause))
         }
       }
      .otherwise {_ =>
         Result.failure(InstallError.IpkgFinding.NotADirectory(module, extractionPath))
       }

}

package install {
  sealed trait InstallError {
    def description: String = toString
  }

  object InstallError {
    sealed trait IpkgFinding extends InstallError
    object IpkgFinding {

      case class NotADirectory(module: AbsolutePath, extractionPath: AbsolutePath) extends IpkgFinding {
        override def description: String =
          s"While trying to locate the ipkg file from inside '$module', it seems that" ` `
          s"the path the files have been extracted to is no longer a directory:" `\n`
          extractionPath.toString
      }

      case class FindInvocationError(
        module:         AbsolutePath,
        extractionPath: AbsolutePath,
        cause:          DirListError)(implicit
        describeCause:  Describe[DirListError]) extends IpkgFinding {
          override def description: String =
            s"Something has gone wrong trying to locate the ipkg file from inside '$module', extracted into" ` `
            s"'$extractionPath'. The reported error is:\n" `\n`
            describeCause.description(cause)
      }

      case class MultipleCandidates(module: AbsolutePath, extractionPath: AbsolutePath, candidates: List[RelativePath]) extends IpkgFinding {
          override def description: String =
            s"There seems to be multiple 'ipkg' files in '$module':\n\n" + candidates.zipWithIndex.map{case (c, i) => s" $i. $c"}.mkString("\n")
      }

      case class NoCandidates(module: AbsolutePath, extractionPath: AbsolutePath) extends IpkgFinding {
          override def description: String =
            s"There seems to be no 'ipkg' files in '$module'"
      }
    }

    implicit val DescribeInstallError: Describe[InstallError] =
      _.description
  }
}

