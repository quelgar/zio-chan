package ziochan

import zio.*
import java.nio.{channels as nc}
import java.io.IOException
import zio.blocking.{ Blocking, blocking }


trait Channel extends IOCloseable:

  protected val nioChannel: nc.Channel

  final def close: IO[IOException, Unit] = ioEffect(nioChannel.close())

  final def isOpen: UIO[Boolean] = ZIO.effectTotal(nioChannel.isOpen())


trait BlockingChannel extends Channel:

  type BlockingOps

  final protected def nioBlocking[R, E, A](effect: ZIO[R, E, A]): ZIO[R & Blocking, E, A] =
    blocking(effect).fork.flatMap(_.join).onInterrupt(close.ignore)

  def useBlocking[R, E >: IOException, A](f: BlockingOps => ZIO[R, E, A]): ZIO[R & Blocking, E, A]
