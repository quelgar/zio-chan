package ziochan
package file

import java.nio.{file as nf}
import ziochan.IOCloseable
import zio.IO
import java.io.IOException

trait Watchable:
  protected def nioWatchable: nf.Watchable

final class WatchService private (nioWs: nf.WatchService) extends IOCloseable:

  def close: IO[IOException, Unit] = ioEffect(nioWs.close())

object WatchService:

  def fromJava(javaWatchService: nf.WatchService): WatchService = WatchService(javaWatchService)
