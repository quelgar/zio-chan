package ziochan

import zio.*
import java.io.IOException

final class IOCloseException(t: Throwable) extends IOException("Resource could not be released", t)

trait IOCloseable:

  def close: IO[IOException, Unit]

extension [R, E, A <: IOCloseable](acquire: ZIO[R, E, A])
  // private[ziochan] inline def ioManagedDie: ZManaged[R, E, A] =
  //   ZManaged.makeInterruptible(acquire)(_.close.orDieWith(new IOCloseException(_)))

  private[ziochan] inline def ioManaged: ZManaged[R, E, A] = ZManaged.makeInterruptible(acquire)(_.close.ignore)

private[ziochan] inline def ioEffect[A](effect: => A): IO[IOException, A] =
  ZIO.effect(effect).refineToOrDie[IOException]

extension [A](maybeNull: A | Null)
  private[ziochan] inline def nullOpt: Option[A] =
    given CanEqual[A | Null, Null] = CanEqual.derived
    if maybeNull == null then None else Some(maybeNull)
