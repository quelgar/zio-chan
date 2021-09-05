package ziochan
package file

import java.nio.{file as nf}
import zio.ZIO
import java.io.File
import java.io.IOError
import zio.IO
import java.nio.file.LinkOption
import java.io.IOException
import zio.Chunk
import scala.jdk.CollectionConverters.*

final class Path private (val toJava: nf.Path) extends AnyVal with Watchable derives CanEqual:

  import Path.*

  override protected inline def nioWatchable = toJava

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case p: Path => toJava.equals(p.toJava)
    case _       => false

  override inline def hashCode: Int = toJava.hashCode

  def /(other: Path): Path = fromJava(toJava.resolve(other.toJava).nn)

  def /(other: String): Path = fromJava(toJava.resolve(other).nn)

  def elements: Chunk[Path] = Chunk.fromIterable(toJava.asScala.map(fromJava))

  def filesystem: FileSystem = FileSystem.fromJava(toJava.getFileSystem.nn)

  def isAbsolute: Boolean = toJava.isAbsolute

  def root: Option[Path] = toJava.getRoot.nullOpt.map(fromJava)

  def filename: Path = fromJava(toJava.getFileName.nn)

  def parent: Option[Path] = toJava.getParent.nullOpt.map(fromJava)

  def nameCount: Int = toJava.getNameCount

  def apply(index: Int): Path = fromJava(toJava.getName(index).nn)

  def subpath(beginIndex: Int, endIndex: Int): Path = fromJava(toJava.subpath(beginIndex, endIndex).nn)

  def startsWith(other: Path): Boolean = toJava.startsWith(other.toJava)

  def endsWith(other: Path): Boolean = toJava.endsWith(other.toJava)

  def normalize: Path = fromJava(toJava.normalize.nn)

  def resolveSibling(other: Path): Path = fromJava(toJava.resolveSibling(other.toJava).nn)

  def resolveSibling(other: String): Path = fromJava(toJava.resolveSibling(other).nn)

  def /^(other: Path): Path = resolveSibling(other)

  def /^(other: String): Path = resolveSibling(other)

  def relativize(other: Path): Path = fromJava(toJava.relativize(other.toJava).nn)

  def toURI: IO[IOError, URI] = ZIO.effect(URI.fromJava(toJava.toUri.nn)).refineToOrDie[IOError]

  def toJavaFile: File = toJava.toFile.nn

  def toAbsolutePath: IO[IOError, Path] = ZIO.effect(fromJava(toJava.toAbsolutePath.nn)).refineToOrDie[IOError]

  def toRealPath(options: LinkOption*): IO[IOException, Path] =
    ioEffect(fromJava(toJava.toRealPath(options*).nn))

end Path

object Path:

  def fromJava(javaPath: nf.Path): Path = Path(javaPath)
