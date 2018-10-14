package ip.zipops

import ip.fileops.path.AbsolutePath
import ip.fileops.path.Path
import ip.fileops.DirCreationError
import ip.result._
import ip.resources
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.util.zip.{ZipException => JZipException}
import java.io.{IOException => JIOException}
import java.io.FileNotFoundException

trait Unzip {

  private def extractEntry(zis: ZipInputStream, target: AbsolutePath)(zipEntry: ZipEntry): Result[ZipEntryExtractionError, Unit] = {
    val fileName = zipEntry.getName
    val buffer = new Array[Byte](1024)
      for {
        path <- (target / fileName).mapError(e => ZipEntryExtractionError.TargetFilePathCanNotBeConstructed(target, fileName, e))
        _    <- path
                  .makeParents
                  .getOrElse( Result.success(()) )
                  .mapError[ZipEntryExtractionError](e => ZipEntryExtractionError.ParentsOfTargetCouldNotBeCreated(path, e))
        _    <- resources
                  .fileOutputStream(path)
                  .mapError {
                    case Left(fnfe) => ZipEntryExtractionError.TargetFileUnnaccessible(path, fnfe)
                    case Right(se) => ZipEntryExtractionError.JavaSecurityExceptionAccessingTarget(path, se)
                   }
                  .flatMap{fos =>
                     try {
                       var len = zis.read(buffer)
                       while (len > 0) {
                         fos.write(buffer, 0, len)
                         len = zis.read(buffer)
                       }
                       Result.success(())
                     }
                     catch {
                       case e: JZipException => Result.failure(ZipEntryExtractionError.ZipException(e))
                       case e: JIOException  => Result.failure(ZipEntryExtractionError.IOException(e))
                     }
                   }
      } yield
        ()
  }

  def unzip(source: AbsolutePath, target: AbsolutePath): Result[UnzipError, Unit] = {

    type R[+T] = Result[UnzipError, T]
    type RU = R[Unit]

    def impl: RU =
      for {
        fis <- resources
                 .fileInputStream(source)
                 .mapError {
                    case Left(fnfe) => UnzipError.SourceUnreadable(source, fnfe)
                    case Right(se)  => UnzipError.JavaSecurityExceptionAccessingSource(source, se)
                  }
        _   <- resources
                 .zipInputStream(fis)
                 .flatMap{zis =>
                    zis
                      .asRunnableStream
                      .mapFalible(extractEntry(zis, target))
                      .run()
                      .asResult
                      .mapError {
                         case Left(gze) => UnzipError.GetZipEntryError(source, gze)
                         case Right(zee) => UnzipError.ZipEntryExtractionError(source, zee)
                       }
                  }
      }
      yield {
        ()
      }

    source.whenFile{_ =>
       target.whenDir{_ =>
          impl
       }
       .otherwise {_ => Result.failure( UnzipError.TargetDoesNotExistOrNotADir(target) ) }
    }
    .otherwise {_ => Result.failure( UnzipError.SourceDoesNotExistOrNotAFile(source) ) }
  }

}

trait ZipEntryExtractionError
object ZipEntryExtractionError {
  case class TargetFilePathCanNotBeConstructed(
    targetRoot: AbsolutePath,
    fileName: String,
    error: Path.ConcatenationError)           extends ZipEntryExtractionError
  case class ZipException(e: JZipException)   extends ZipEntryExtractionError
  case class IOException(e: JIOException)     extends ZipEntryExtractionError
  case class TargetFileUnnaccessible(
    fullTargetPath: AbsolutePath,
    cause: FileNotFoundException)             extends ZipEntryExtractionError
  case class JavaSecurityExceptionAccessingTarget(
    fullTargetPath: AbsolutePath,
    cause: SecurityException)                 extends ZipEntryExtractionError
  case class ParentsOfTargetCouldNotBeCreated(
    pathToFileWhoseParentsCouldNotBeCreated: AbsolutePath,
    cause: DirCreationError)                  extends ZipEntryExtractionError
}

