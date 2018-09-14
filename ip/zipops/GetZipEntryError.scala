package ip.zipops

import java.io.{IOException => JIOException}
import java.util.zip.{ZipException => JZipException}

sealed trait GetZipEntryError
object GetZipEntryError {
  case class ZipException(e: JZipException)   extends GetZipEntryError
  case class IOException(e: JIOException)     extends GetZipEntryError
}
