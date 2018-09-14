package ip.zipops

import ip.fileops.path.AbsolutePath
import java.io.FileNotFoundException
import java.lang.SecurityException

sealed trait UnzipError
object UnzipError {

  case class SourceDoesNotExistOrNotAFile(
                source: AbsolutePath)                     extends UnzipError

  case class TargetDoesNotExistOrNotADir(
                source: AbsolutePath)                     extends UnzipError

  case class SourceUnreadable(
                source: AbsolutePath,
                error:  FileNotFoundException)            extends UnzipError

  case class JavaSecurityExceptionAccessingSource(
                source: AbsolutePath,
                e:      SecurityException)                extends UnzipError

  case class GetZipEntryError(
                source: AbsolutePath,
                error: ip.zipops.GetZipEntryError)        extends UnzipError

  case class ZipEntryExtractionError(
                source: AbsolutePath,
                error: ip.zipops.ZipEntryExtractionError) extends UnzipError
}

