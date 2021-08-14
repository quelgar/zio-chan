package ziochan
package file

import java.nio.{file as nf}
import ziochan.IOCloseable
import java.io.IOException
import zio.ZIO
import zio.IO
import zio.UIO
import scala.jdk.CollectionConverters.*
import java.nio.file.attribute.UserPrincipalLookupService
import zio.ZManaged
import zio.blocking.{Blocking, effectBlockingIO, effectBlocking}
import java.nio.file.FileSystemNotFoundException
import java.nio.file.ProviderNotFoundException
import java.net.URI

final class FileSystem private (nioFs: nf.FileSystem) extends IOCloseable:

  override inline def close: IO[IOException, Unit] = ioEffect(nioFs.close())

  inline def provider: nf.spi.FileSystemProvider = nioFs.provider().nn

  inline def isOpen: UIO[Boolean] = ZIO.effectTotal(nioFs.isOpen())

  inline def isReadOnly: Boolean = nioFs.isReadOnly

  inline def getSeparator: String = nioFs.getSeparator.nn

  def getRootDirectories: UIO[List[Path]] =
    ZIO.effectTotal(nioFs.getRootDirectories.nn.asScala.toList.map(Path.fromJava))

  inline def getFileStores: UIO[List[FileStore]] =
    UIO.effectTotal(nioFs.getFileStores.nn.asScala.toList.map(FileStore.fromJava))

  inline def supportedFileAttributeViews: Set[String] = nioFs.supportedFileAttributeViews.nn.asScala.toSet

  inline def getPath(first: String, more: String*): Path = Path.fromJava(nioFs.getPath(first, more*).nn)

  inline def getPathMatcher(syntaxAndPattern: String): nf.PathMatcher = nioFs.getPathMatcher(syntaxAndPattern).nn

  inline def getUserPrincipalLookupService: UserPrincipalLookupService = nioFs.getUserPrincipalLookupService.nn

  inline def newWatchService: ZManaged[Blocking, IOException, WatchService] = effectBlockingIO(
    WatchService.fromJava(nioFs.newWatchService().nn)
  ).ioManaged

end FileSystem

object FileSystem:

  def fromJava(javaFileSystem: nf.FileSystem): FileSystem = FileSystem(javaFileSystem)

  val default: FileSystem = fromJava(nf.FileSystems.getDefault.nn)

  def getFileSystem(uri: URI): ZIO[Blocking, FileSystemNotFoundException | ProviderNotFoundException, FileSystem] =
    effectBlocking(fromJava(nf.FileSystems.getFileSystem(uri).nn))
      .refineOrDie {
        case e: FileSystemNotFoundException => e
        case e: ProviderNotFoundException   => e
      }

  def newFileSystem(
    uri: URI,
    loader: ClassLoader,
    env: Map[String, ?]
  ): ZManaged[Blocking, FileSystemNotFoundException | ProviderNotFoundException | IOException, FileSystem] =
    effectBlocking(fromJava(nf.FileSystems.newFileSystem(uri, env.asJava, loader).nn))
      .refineOrDie[FileSystemNotFoundException | ProviderNotFoundException | IOException] {
        case e: FileSystemNotFoundException => e
        case e: ProviderNotFoundException   => e
        case e: IOException                 => e
      }
      .ioManaged

  def newFileSystem(
    uri: URI,
    loader: ClassLoader,
    env: (String, Any)*
  ): ZManaged[Blocking, FileSystemNotFoundException | ProviderNotFoundException | IOException, FileSystem] =
    newFileSystem(uri, loader, env.toMap)

  def newFileSystem(
    uri: URI,
    env: Map[String, ?]
  ): ZManaged[Blocking, FileSystemNotFoundException | ProviderNotFoundException | IOException, FileSystem] =
    effectBlocking(fromJava(nf.FileSystems.newFileSystem(uri, env.asJava).nn))
      .refineOrDie[FileSystemNotFoundException | ProviderNotFoundException | IOException] {
        case e: FileSystemNotFoundException => e
        case e: ProviderNotFoundException   => e
        case e: IOException                 => e
      }
      .ioManaged

  def newFileSystem(
    uri: URI,
    env: (String, Any)*
  ): ZManaged[Blocking, FileSystemNotFoundException | ProviderNotFoundException | IOException, FileSystem] =
    newFileSystem(uri, env.toMap)

  def newFileSystem(
    path: Path,
    loader: ClassLoader
  ): ZManaged[Blocking, ProviderNotFoundException | IOException, FileSystem] =
    effectBlocking(fromJava(nf.FileSystems.newFileSystem(path.toJava, loader).nn))
      .refineOrDie[ProviderNotFoundException | IOException] {
        case e: ProviderNotFoundException => e
        case e: IOException               => e
      }
      .ioManaged

end FileSystem
