package ziochan

import zio.*
import java.io.IOException
import scala.reflect.ClassTag

final class IOCloseException(t: Throwable) extends IOException("Resource could not be released", t)

trait IOCloseable:

  def close: IO[IOException, Unit]

extension [R, E, A <: IOCloseable](acquire: ZIO[R, E, A])
  // private[ziochan] inline def ioManagedDie: ZManaged[R, E, A] =
  //   ZManaged.makeInterruptible(acquire)(_.close.orDieWith(IOCloseException(_)))

  private[ziochan] inline def ioManaged: ZManaged[R, E, A] = ZManaged.makeInterruptible(acquire)(_.close.ignore)

private[ziochan] inline def ioEffect[A](effect: => A): IO[IOException, A] =
  ZIO.effect(effect).refineToOrDie[IOException]

extension [A](maybeNull: A | Null)
  private[ziochan] inline def nullOpt: Option[A] =
    given CanEqual[A | Null, Null] = CanEqual.derived
    if maybeNull == null then None else Some(maybeNull)

private[ziochan] final class Catcher[A](thunk: () => A):
  inline def catching[E <: Throwable](using classTag: ClassTag[E]): Either[E, A] =
    try Right(thunk())
    catch case classTag(e) => Left(e)

private[ziochan] inline def in[A](thunk: => A): Catcher[A] = Catcher(() => thunk)

private[ziochan] inline def catching[A, E](thunk: => A)(c: PartialFunction[Throwable, E]): Either[E, A] =
  try Right(thunk)
  catch c.andThen(Left(_))

private[ziochan] def foreachEither[E, A, B](xs: Iterable[A])(f: A => Either[E, B]): Either[E, Chunk[B]] =
  xs.foldLeft(Right(Chunk.empty): Either[E, Chunk[B]]) { (acc, a) =>
    acc.flatMap(chunk => f(a).map(chunk :+ _))
  }
