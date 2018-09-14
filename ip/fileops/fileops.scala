package ip

import java.nio.file.{Files => JFiles}

import ip.fileops.path._

package fileops {

  sealed trait ReadError
  object ReadError {

    case class  NoSuchFile(path: AbsolutePath) extends ReadError

    case object IO                             extends ReadError
    case object OutOfMemory                    extends ReadError
    case object JavaSecurity                   extends ReadError

    def apply(path: AbsolutePath, t: Throwable): ReadError =
      t match {
        case _: java.nio.file.NoSuchFileException => NoSuchFile(path)
        case _: java.io.IOException               => IO
        case _: java.lang.OutOfMemoryError        => OutOfMemory
        case _: java.lang.SecurityException       => JavaSecurity
      }
  }
 
  sealed trait FileSystemRef{val path: AbsolutePath}
  case class FileRef    (path: AbsolutePath) extends FileSystemRef {
    def rm: Either[FileRemoveError, Unit] =
      try {
        path.toJava.toFile.delete
        Right(())
      } catch {
        case e: java.lang.SecurityException =>
          Left(FileRemoveError.JavaSecurityException(e))
      }
  }

  case class DirRef     (path: AbsolutePath) extends FileSystemRef {
    def mkdirs: Either[DirCreationError, Boolean] =
      try {
        Right(path.toJava.toFile.mkdirs)
      } catch {
        case e: java.lang.SecurityException =>
          Left(DirCreationError.JavaSecurityException(e))
      }
    def list: Either[DirListError, List[RelativePath]] = {
      try {
        val listOfFiles = path.toJava.toFile.listFiles.toList

        Right( listOfFiles.map(f => new RelativePath(f.toPath)))
      }
      catch {
        case otherwise: Throwable =>
          throw otherwise
      }
    }
    def find(regexFilenamePattern: String): Either[DirListError, List[RelativePath]] = {
      val regex = regexFilenamePattern.r
      list
        .map{
           _.filter{
             _.toString match {
                 case regex(_*) => true
                 case _ => false
               }
             }
         }
    }

  }

  case class NothingRef (path: AbsolutePath) extends FileSystemRef {
    def mkdir: Either[DirCreationError, Boolean] =
        try {
          Right(path.toJava.toFile.mkdir)
        } catch {
          case e: java.lang.SecurityException =>
            Left(DirCreationError.JavaSecurityException(e))
        }
    def mkdirs: Either[DirCreationError, Boolean] =
        try {
          Right(path.toJava.toFile.mkdirs)
        } catch {
          case e: java.lang.SecurityException =>
            Left(DirCreationError.JavaSecurityException(e))
        }
  }

  class StartWithFile [T](path: AbsolutePath, fop: FileRef => T) {
    case class whenDir[U >: T](dop: DirRef => U) {
      def whenNothing[V >: U](nop: NothingRef => V): V = processFileSystemRefOp[V](path, fop, dop, nop)
    }
    case class whenNothing[U >: T](nop: NothingRef => U) {
      def whenDir[V >: U](dop: DirRef => V): V = processFileSystemRefOp[V](path, fop, dop, nop)
    }
    def otherwise[U >: T](op: AbsolutePath => U): U =
      if (path.isFile) fop(FileRef(path))
      else op(path)
  }
  class StartWithDir [T](path: AbsolutePath, dop: DirRef => T) {
    case class whenFile[U >: T](fop: FileRef => U) {
      def whenNothing[V >: U](nop: NothingRef => V): V = processFileSystemRefOp[V](path, fop, dop, nop)
    }
    case class whenNothing[U >: T](nop: NothingRef => U) {
      def whenFile[V >: U](fop: FileRef => V): V = processFileSystemRefOp[V](path, fop, dop, nop)
    }
    def otherwise[U >: T](op: AbsolutePath => U): U =
      if (path.isDirectory) dop(DirRef(path))
      else op(path)
  }
  class StartWithNothing [T](path: AbsolutePath, nop: NothingRef => T) {
    case class whenFile[U >: T](fop: FileRef => U) {
      def whenDir[V >: U](dop: DirRef => V): V = processFileSystemRefOp[V](path, fop, dop, nop)
    }
    case class whenDir[U >: T](dop: DirRef => U) {
      def whenFile[V >: U](fop: FileRef => V): V = processFileSystemRefOp[V](path, fop, dop, nop)
    }
    def otherwise[U >: T](op: AbsolutePath => U): U =
      if (path.doesNotExist) nop(NothingRef(path))
      else op(path)
  }

  sealed trait DirCreationError
  object DirCreationError {
    case class JavaSecurityException(cause: java.lang.SecurityException) extends DirCreationError
    case class ItsAFile(path: AbsolutePath) extends DirCreationError
  }

  sealed trait FileRemoveError
  object FileRemoveError {
    case class JavaSecurityException(cause: java.lang.SecurityException) extends FileRemoveError
  }

  sealed trait DirListError
  object DirListError {
  }
}

package object fileops {

  def readUTF8(path: AbsolutePath): Either[ReadError, String] =
    readAllBytes(path).map(new String(_, "UTF-8"))


  def readAllBytes(path: AbsolutePath): Either[ReadError, Array[Byte]] =
    try {
      Right(JFiles.readAllBytes(path.toJava))
    }
    catch {
      case t: Throwable =>
        Left(ReadError(path, t))
    }

  implicit class FileSystemOps(val path: AbsolutePath) extends AnyVal {
    def exists : Boolean =
      path.toJava.toFile.exists

    def doesNotExist : Boolean =
      !exists

    def isDirectory: Boolean =
      path.toJava.toFile.isDirectory

    def isNoDirectory: Boolean =
      !isDirectory

    def isFile: Boolean =
      path.toJava.toFile.isFile

    def makeParents : Option[Either[DirCreationError, Boolean]] = {
      path.parent map {p =>
        p.whenDir     (_.mkdirs)
         .whenNothing (_.mkdirs)
         .whenFile    (_ => Left(DirCreationError.ItsAFile(p)))
      }
    }
  }


  implicit class FileSystemReferenceStarterOps(val path: AbsolutePath) extends AnyVal {
    def whenFile[T]   (fop: FileRef    => T): StartWithFile[T]    = new StartWithFile(path, fop)
    def whenDir[T]    (dop: DirRef     => T): StartWithDir[T]     = new StartWithDir(path, dop)
    def whenNothing[T](nop: NothingRef => T): StartWithNothing[T] = new StartWithNothing(path, nop)
  }

  private[fileops] def processFileSystemRefOp[T](
    path: AbsolutePath,
    fop: FileRef    => T,
    dop: DirRef     => T,
    nop: NothingRef => T): T = {
           if (path.doesNotExist) nop(NothingRef(path))
      else if (path.isDirectory)  dop(DirRef(path))
      else if (path.isFile)       fop(FileRef(path))
      else                        throw new Exception(s"FATAL: '$path' seems to exist but it's no file or directory")
  }
}




