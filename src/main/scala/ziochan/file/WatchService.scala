package ziochan
package file

import java.nio.{file as nf}
import ziochan.IOCloseable
import zio.*
import java.io.IOException
import java.util.concurrent.TimeUnit
import zio.duration.*
import zio.blocking.Blocking
import zio.stream.ZStream

trait Watchable extends Any:
  protected def nioWatchable: nf.Watchable

  final def register(watcher: WatchService, events: nf.WatchEvent.Kind[?]*): IO[IOException, WatchKey] =
    ioEffect(WatchKey.fromJava(nioWatchable.register(watcher.nioWs, events*).nn))

  final def register(
    watcher: WatchService,
    events: Iterable[nf.WatchEvent.Kind[?]],
    modifiers: nf.WatchEvent.Modifier*
  ): IO[IOException, WatchKey] =
    ioEffect(
      WatchKey.fromJava(
        nioWatchable.register(watcher.nioWs, events.toArray[nf.WatchEvent.Kind[?] | Null], modifiers*).nn
      )
    )

object Watchable:
  def fromJava(javaWatchable: nf.Watchable): Watchable = new Watchable {
    override protected def nioWatchable = javaWatchable
  }

extension (watchEvent: nf.WatchEvent[?])
  def asPath: Option[Path] = watchEvent.context().nullOpt match
    case Some(javaPath: nf.Path) => Some(Path.fromJava(javaPath))
    case _                       => None

opaque type WatchKey = nf.WatchKey

extension (watch: WatchKey)
  def isValid: UIO[Boolean] = ZIO.effectTotal(watch.isValid)
  def pollEvents: UIO[Chunk[nf.WatchEvent[?]]] = ???
  def pollEventsManaged: UManaged[Chunk[nf.WatchEvent[?]]] =
    this.pollEvents(watch).toManaged_.ensuring(this.reset(watch))
  def reset: UIO[Boolean] = ZIO.effectTotal(watch.reset())
  def cancel: UIO[Unit] = UIO.effectTotal(watch.cancel())
  def watchable: Watchable = watch.watchable.nn match
    case javaPath: nf.Path => Path.fromJava(javaPath)
    case javaWatchable     => Watchable.fromJava(javaWatchable)
  def watchablePath: Option[Path] = watch.watchable.nn match
    case javaPath: nf.Path => Some(Path.fromJava(javaPath))
    case _                 => None
  def resolveEventPath(event: nf.WatchEvent[?]): Option[Path] =
    for
      parent <- watch.watchable.nn match
        case javaPath: nf.Path => Some(Path.fromJava(javaPath))
        case _                 => None
      eventPath <- event.asPath
    yield parent / eventPath

object WatchKey:

  def fromJava(javaWatchKey: nf.WatchKey): WatchKey = javaWatchKey

final class WatchService private (private[file] val nioWs: nf.WatchService) extends IOCloseable:

  def close: IO[IOException, Unit] = ioEffect(nioWs.close())

  def poll: UIO[Option[WatchKey]] = ZIO.effectTotal(nioWs.poll().nullOpt.map(WatchKey.fromJava))

  def poll(timeout: Duration): URIO[Blocking, Option[WatchKey]] =
    blocking
      .effectBlockingInterrupt(nioWs.poll(timeout.toNanos, TimeUnit.NANOSECONDS).nullOpt.map(WatchKey.fromJava))
      .orDie

  def take: URIO[Blocking, WatchKey] = blocking.effectBlockingInterrupt(WatchKey.fromJava(nioWs.take().nn)).orDie

  def stream: ZStream[Blocking, Nothing, WatchKey] = ZStream.repeatEffect(take)

object WatchService:

  def fromJava(javaWatchService: nf.WatchService): WatchService = WatchService(javaWatchService)

  def forDefaultFileSystem: ZManaged[Blocking, IOException, WatchService] = FileSystem.default.newWatchService
