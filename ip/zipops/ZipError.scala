package ip.zipops

import ip.fileops.path._

import java.io.FileNotFoundException
import java.lang.SecurityException

sealed trait ZipError
object ZipError {

  case object TargetAlreadyExists                         extends ZipError

  case class  MissingSources(
                missingSources: List[AbsolutePath])       extends ZipError

  case class  UnwriteableTarget(
                target:         AbsolutePath,
                e:              FileNotFoundException)    extends ZipError

  case class  JavaSecurityExceptionAccessingTarget(
                target:         AbsolutePath,
                e:              SecurityException)        extends ZipError

  case class  FailureAddingFileToZip(
                target:         AbsolutePath,
                source:         AbsolutePath,
                entryName:      RelativePath,
                error:          PutToZipError)            extends ZipError

  case class  InputSourceUnreadable(
                target:         AbsolutePath,
                source:         AbsolutePath,
                entryName:      RelativePath,
                error:          FileNotFoundException)    extends ZipError

  case class  JavaSecurityExceptionAccessingInputSource(
                target:         AbsolutePath,
                source:         AbsolutePath,
                entryName:      RelativePath,
                error:          SecurityException)        extends ZipError

}
