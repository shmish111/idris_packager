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

import ip.result._
import ip.fileops.path._

import scala.util.{Failure, Success, Try}

import ip.terminate._

package object resources {

  type ||[A, B] = Either[A, B]

  def fileOutputStream(path: AbsolutePath): Result[FileNotFoundException || SecurityException, FileOutputStream] =
    Result
      .success{Try(new FileOutputStream(path.toJava.toFile))}
      .flatMap{
         case Success(fos)                        => Result.create(Right(fos), () => fos.close())
         case Failure(e: FileNotFoundException)   => Result.create(Left(Left(e)), () => ())
         case Failure(e: SecurityException)       => Result.create(Left(Right(e)), () => ())
         case Failure(e)                          => fatal("Trying to create a 'FileOutputStream', the JVM has thrown an undocumented exception", e)
       }

  def fileInputStream(path: AbsolutePath): Result[FileNotFoundException || SecurityException, FileInputStream] =
    Result
      .success{Try(new FileInputStream(path.toJava.toFile))}
      .flatMap{
         case Success(fis)                        => Result.create(Right(fis), () => fis.close())
         case Failure(e: FileNotFoundException)   => Result.create(Left(Left(e)), () => ())
         case Failure(e: SecurityException)       => Result.create(Left(Right(e)), () => ())
         case Failure(e)                          => fatal("Trying to create a 'FileInputStream', the JVM has thrown an undocumented exception", e)
       }

  def zipOutputStream(os: OutputStream): Result[Nothing, ZipOutputStream] = {
    val zos = new ZipOutputStream(os)
    Result.create(Right(zos), () => zos.close)
  }

  def zipInputStream(is: InputStream): Result[Nothing, ZipInputStream] = {
    val zis = new ZipInputStream(is)
    Result.create(Right(zis), () => zis.close)
  }

  def temporaryDirectory(): Result[IOException || SecurityException, AbsolutePath] = {
    Try{Files.createTempDirectory(null)} match {
      case Success(jpath)                 =>
        Path(jpath) match {
          case _: RelativePath =>
            impossible(
              s"""|The invocation 'Files.createTempDirectory(null)', should always return an
                  |absolute path, but, for some reason, we have detected it's response
                  |'$jpath' as a relative one instead""".stripMargin)
          case ap: AbsolutePath =>
            Result.create(Right(ap), () => ())
        }
      case Failure(e: IOException)        => Result.create(Left(Left(e)), () => ())
      case Failure(e: SecurityException)  => Result.create(Left(Right(e)), () => ())
      case Failure(e)                     => fatal("""|Trying to generate a temporal directory with
                                                      |'Files.createTempDirectory(null)', the JVM has thrown
                                                      |an undocumented exception""".stripMargin, e)
    }
  }
}
