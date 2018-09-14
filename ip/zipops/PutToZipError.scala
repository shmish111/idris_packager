package ip.zipops

import java.util.zip.{ZipException => JZipException}
import java.io.{IOException => JIOException}

sealed trait PutToZipError
object PutToZipError {
  case class IncorrectEntryName(t: Throwable) extends PutToZipError
  case class ZipException(e: JZipException)   extends PutToZipError
  case class IOException(e: JIOException)     extends PutToZipError
}
