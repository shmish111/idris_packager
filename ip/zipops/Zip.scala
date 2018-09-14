package ip.zipops

import ip.fileops.path._
import ip.resource._
import ip.eitherops.EUnit
import ip.resources



trait Zip {

  def zip(target: AbsolutePath, root: AbsolutePath, content: List[RelativePath]): EUnit[ZipError] = {

    val result: RUnit[ZipError] =
      for {
        targetPath      <- target
                             .when(! _.exists)
                             .orFail[ZipError](ZipError.TargetAlreadyExists)
        srcsAndNames     = content.map(f => (root / f, f))
        _               <- srcsAndNames
                             .map(_._1)
                             .filterNot(_.exists)
                             .when(_.isEmpty)
                             .orFailWith(ZipError.MissingSources(_))
        fos             <- resources.fileOutputStream(target)
                             .mapError {
                                case Right(e) => ZipError.JavaSecurityExceptionAccessingTarget(target, e)
                                case Left(e)  => ZipError.UnwriteableTarget(target, e)
                              }
        zipOut          <- resources.zipOutputStream(fos)
        _               <- srcsAndNames
                             .map{case (source, name) =>
                                resources
                                  .fileInputStream(source).map((_, name))
                                  .mapError {
                                     case Left(fnf) =>
                                       ZipError.InputSourceUnreadable(
                                         target, root / name, name, fnf)
                                     case Right(se) =>
                                       ZipError.JavaSecurityExceptionAccessingInputSource(
                                         target, root / name, name, se)
                                   }
                                  .flatMap[ZipError, Unit]{ case (fis, name) =>
                                     zipOut
                                      .put(name, fis)
                                      .mapError(e =>
                                         ZipError.FailureAddingFileToZip(
                                           target, root / name, name, e))
                                   }
                              }
                             .failFastSequence
      } yield {
        ()
      }

    result.run

  }

}
