package ziochan

import zio.*
import ziochan.buffers.*

object Test extends App:

  override def run(args: List[String]) =
    ByteOrder.BigEndian == ByteOrder.BigEndian
    val program = for
      buf1 <- Buffer.byte.allocate(100)
      cb1 <- buf1.asCharBuffer
      pos1 <- cb1.position
      limit1 <- cb1.limit
      _ <- console.putStrLn(s"pos=$pos1, limit=$limit1")
      _ <- cb1.put('A')
      _ <- cb1.put('B')
      _ <- cb1.put('ة')
      pos2 <- cb1.position
      _ <- buf1.limit(pos2 * 2)
      bytes1 <- buf1.getChunk()
      _ <- console.putStrLn(s"bytes1 = ${bytes1}")
      _ <- buf1.position(0)
      _ <- buf1.useJava(javaBuf => ZIO.effectTotal(println(s"using Java buffer: ${javaBuf.get()}")))
      _ <- cb1.useJava(javaBuf => ZIO.effectTotal(println(s"using Java char buffer: ${javaBuf.remaining()}")))
      _ <- buf1.position(0)
      c <- buf1.getChar
      _ <- console.putStrLn(s"char=$c")
    yield ()

    program.exitCode

end Test
