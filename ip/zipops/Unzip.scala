package ip.zipops

import ip.eitherops._
import ip.fileops.path.AbsolutePath
import ip.fileops.path.Path.PathConcatenationError
import ip.fileops.DirCreationError
import ip.resource._
import ip.resources
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.util.zip.{ZipException => JZipException}
import java.io.{IOException => JIOException}
import java.io.FileNotFoundException

trait Unzip {

  private def extractEntry(zis: ZipInputStream, target: AbsolutePath)(zipEntry: ZipEntry): Either[ZipEntryExtractionError, Unit] = {
    val fileName = zipEntry.getName
    val buffer = new Array[Byte](1024)
    val r =
      for {
        path <- (target / fileName).mapError(e => ZipEntryExtractionError.TargetFilePathCanNotBeConstructed(target, fileName, e)).asResource
        _    <- path
                  .makeParents
                  .map((e: Either[DirCreationError, Boolean]) => Resource(e))
                  .getOrElse( Resource.success(()) )
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
                       Resource.success(())
                     }
                     catch {
                       case e: JZipException => Resource.failure(ZipEntryExtractionError.ZipException(e))
                       case e: JIOException  => Resource.failure(ZipEntryExtractionError.IOException(e))
                     }
                   }
      } yield
        ()
    r.run()
  }

  def unzip(source: AbsolutePath, target: AbsolutePath): EUnit[UnzipError] = {

    type R[+T] = Resource[UnzipError, T]
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
                      .run.asResource
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
          impl.run
       }
       .otherwise {_ => Left( UnzipError.TargetDoesNotExistOrNotADir(target) ) }
    }
    .otherwise {_ => Left( UnzipError.SourceDoesNotExistOrNotAFile(source) ) }
  }

}

trait ZipEntryExtractionError
object ZipEntryExtractionError {
  case class TargetFilePathCanNotBeConstructed(
    targetRoot: AbsolutePath,
    fileName: String,
    error: PathConcatenationError)            extends ZipEntryExtractionError
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

