package ziochan
package file

import java.nio.{file as nf}
import zio.*
import java.io.IOException
import java.nio.file.attribute.FileAttributeView
import scala.reflect.ClassTag
import java.nio.file.attribute.FileStoreAttributeView

final class FileStore private (nioFs: nf.FileStore):

  import FileStore.*

  inline def name: String = nioFs.name.nn

  inline def storeType: String = nioFs.`type`.nn

  inline def isReadOnly: Boolean = nioFs.isReadOnly

  inline def totalSpace: IO[IOException, Long] = ioEffect(nioFs.getTotalSpace())

  inline def usableSpace: IO[IOException, Long] = ioEffect(nioFs.getUsableSpace())

  inline def unallocatedSpace: IO[IOException, Long] = ioEffect(nioFs.getUnallocatedSpace())

  inline def blockSize: IO[IOException, Long] = ioEffect(nioFs.getBlockSize())

  inline def supportsFileAttributeView[A <: FileAttributeView](using classTag: ClassTag[A]): Boolean =
    nioFs.supportsFileAttributeView(classTag.runtimeClass.asInstanceOf[Class[? <: FileAttributeView]])

  inline def supportsFileAttributeView(name: String): Boolean = nioFs.supportsFileAttributeView(name)

  inline def getFileStoreAttributeView[A <: FileStoreAttributeView](using classTag: ClassTag[A]): Option[A] =
    nioFs.getFileStoreAttributeView(classTag.runtimeClass.asInstanceOf[Class[A]]).nullOpt

  inline def getAttribute(attribute: Attribute): IO[IOException, Option[Any]] = ioEffect(
    nioFs.getAttribute(name).nullOpt
  )

  inline def getAttribute(viewName: String, attributeName: String): IO[IOException, Option[Any]] = getAttribute(
    Attribute(viewName, attributeName)
  )

object FileStore:

  final case class Attribute(viewName: String, attributeName: String):
    def javaString: String = s"$viewName:$attributeName"

  def fromJava(javaFileStore: nf.FileStore): FileStore = new FileStore(javaFileStore)
