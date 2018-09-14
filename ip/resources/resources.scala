package ip

import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.OutputStream
import java.io.InputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.lang.SecurityException
import java.util.zip.ZipOutputStream
import java.util.zip.ZipInputStream
import java.nio.file.Files

import ip.resource._
import ip.fileops.path._

import scala.util.{Failure, Success, Try}

package object resources {

  type ||[A, B] = Either[A, B]

  def fileOutputStream(path: AbsolutePath): Resource[FileNotFoundException || SecurityException, FileOutputStream] =
    Resource
      .success{Try(new FileOutputStream(path.toJava.toFile))}
      .flatMap{
         case Success(fos)                        => Resource.create(Right(fos), () => fos.close())
         case Failure(e: FileNotFoundException)   => Resource.create(Left(Left(e)), () => ())
         case Failure(e: SecurityException)       => Resource.create(Left(Right(e)), () => ())
         case Failure(e)                          => throw new RuntimeException("Unexpected exception generating a FileOutputStream", e)
       }

  def fileInputStream(path: AbsolutePath): Resource[FileNotFoundException || SecurityException, FileInputStream] =
    Resource
      .success{Try(new FileInputStream(path.toJava.toFile))}
      .flatMap{
         case Success(fis)                        => Resource.create(Right(fis), () => fis.close())
         case Failure(e: FileNotFoundException)   => Resource.create(Left(Left(e)), () => ())
         case Failure(e: SecurityException)       => Resource.create(Left(Right(e)), () => ())
         case Failure(e)                          => throw new RuntimeException("Unexpected exception generating a FileInputStream", e)
       }

  def zipOutputStream(os: OutputStream): Resource[Nothing, ZipOutputStream] = {
    val zos = new ZipOutputStream(os)
    Resource.create(Right(zos), () => zos.close)
  }

  def zipInputStream(is: InputStream): Resource[Nothing, ZipInputStream] = {
    val zis = new ZipInputStream(is)
    Resource.create(Right(zis), () => zis.close)
  }

  def temporaryDirectory(): Resource[IOException || SecurityException, AbsolutePath] = {
    Try{Files.createTempDirectory(null)} match {
      case Success(jpath)                 =>
        Path(jpath) match {
          case _: RelativePath =>
            throw new RuntimeException("The JVM has returned, unexpectedly, a non absolute path from a method that should always return one")
          case ap: AbsolutePath =>
            Resource.create(Right(ap), () => ())
        }
      case Failure(e: IOException)        => Resource.create(Left(Left(e)), () => ())
      case Failure(e: SecurityException)  => Resource.create(Left(Right(e)), () => ())
      case Failure(e)                     => throw new RuntimeException("Unexpected exception generating a temporary directory", e)
    }
  }
}
