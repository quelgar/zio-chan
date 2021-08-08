package ziochan.file

import java.nio.{file as nf}
import zio.ZIO

final class Path private (private val nioPath: nf.Path) extends Watchable derives CanEqual:

  import Path.*

  override protected inline def nioWatchable = nioPath

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case p: Path => nioPath.equals(p.nioPath)
    case _       => false

  override inline def hashCode: Int = nioPath.hashCode

  def withJava[R, E, A](f: nf.Path => ZIO[R, E, A]): ZIO[R, E, A] = f(nioPath)

  def /(other: Path): Path = fromJava(nioPath.resolve(other.nioPath).nn)

  def /(other: String): Path = fromJava(nioPath.resolve(other).nn)

  def filesystem: FileSystem = ???

object Path {

  def fromJava(javaPath: nf.Path): Path = new Path(javaPath)

}
