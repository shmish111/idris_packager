package ip

import ip.fileops.path._
import ip.result._
import ip.runnablestream._

import scala.util.{Failure, Success, Try}

import java.io.InputStream
import java.io.{IOException => JIOException}
import java.util.zip.ZipOutputStream
import java.util.zip.ZipInputStream
import java.util.zip.ZipEntry
import java.util.zip.{ZipException => JZipException}

package object zipops extends Zip
                         with Unzip {

  implicit class ZipOutputStreamOps(val zos: ZipOutputStream) extends AnyVal {
    def put(name: RelativePath, content: InputStream): RUnit[PutToZipError] = {
      Result
        .success(Try{new ZipEntry(name.toString)})
        .flatMap {
            case Success(ze) => Result.success(ze)
            case Failure(f)  => Result.failure(PutToZipError.IncorrectEntryName(f))
         }
        .flatMap {ze =>
           try {
             zos.putNextEntry(ze)
             val bytes = new Array[Byte](1024)
             var length = content.read(bytes)
             while(length >= 0) {
               zos.write(bytes, 0, length);
               length = content.read(bytes)
             }
             Result.success(())
           }
           catch {
             case e: JZipException => Result.failure(PutToZipError.ZipException(e))
             case e: JIOException  => Result.failure(PutToZipError.IOException(e))
           }
         }
    }
  }

  implicit class ZipInputStreamOps(val zis: ZipInputStream) extends AnyVal {

    def asRunnableStream: RunnableStream[GetZipEntryError, ZipEntry] = new RunnableStream[GetZipEntryError, ZipEntry] {
      override def clean(): Unit = {
        import scala.util._
        Try { zis.closeEntry }
        ()
      }
      override def next: Either[GetZipEntryError, Option[ZipEntry]] = {
        import scala.util._
        Try { zis.getNextEntry } match {
          case Success(ne) if ne == null =>
            Right(None)
          case Success(ne) =>
            Right(Some(ne))
          case Failure(e: JZipException) =>
             Left(GetZipEntryError.ZipException(e))
          case Failure(e: JIOException) =>
             Left(GetZipEntryError.IOException(e))
          case Failure(otherwise) =>
            // This exception has not been reported by the getNextEntry javadocs
            throw otherwise
        }
      }
    }
  }

}
