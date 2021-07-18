package ziochan.buffers

import java.nio
import zio.*
import zio.blocking.Blocking
import scala.reflect.ClassTag
import scala.annotation.targetName

enum ByteOrder derives CanEqual:
  case BigEndian, LittleEndian

private given CanEqual[nio.ByteOrder, nio.ByteOrder] = CanEqual.derived

abstract class Buffer[@specialized A] private[buffers] ():

  import Buffer.*

  protected[buffers] val nioBuffer: nio.Buffer

  // *** Specific to each supported A

  protected def unsafeGetArray(): Array[A] | Null

  protected def unsafePutArray(a: Array[A]): Unit

  protected def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[A]

  protected def unsafeGetOrder(): nio.ByteOrder | Null

  protected def unsafeCompact(): Any

  protected def unsafeAsReadOnlyBuffer(): nio.Buffer | Null

  protected def unsafeGet(): A

  protected def unsafeGet(index: Int): A

  protected def unsafeGet(a: Array[A]): Any

  protected def unsafePut(element: A): Any

  protected def unsafePut(index: Int, element: A): Any

  protected def unsafePutBuffer(src: Buffer[A]): Any

  protected def unsafeMismatch(other: Buffer[A]): Int

  // *** Public API

  final def asReadOnlyBuffer: UIO[Buffer[A]] = ZIO.effectTotal(unsafeFromNio(unsafeAsReadOnlyBuffer().nn))

  final def compact: UIO[Unit] = ZIO.effectTotal(unsafeCompact()).unit

  final def get: UIO[A] = ZIO.effectTotal(unsafeGet())

  final def get(index: Int): UIO[A] = ZIO.effectTotal(unsafeGet(index))

  final def getChunk(maxLength: Int = Int.MaxValue)(using ClassTag[A]): UIO[Chunk[A]] = ZIO.effectTotal {
    val array = Array.ofDim[A](math.min(maxLength, nioBuffer.remaining()))
    unsafeGet(array)
    Chunk.fromArray(array)
  }

  final def put(element: A): UIO[Unit] = ZIO.effectTotal(unsafePut(element)).unit

  final def put(index: Int, element: A): UIO[Unit] = ZIO.effectTotal(unsafePut(index, element)).unit

  final def putBuffer(src: Buffer[A]): UIO[Unit] = ZIO.effectTotal(unsafePutBuffer(src)).unit

  final def order: UIO[ByteOrder] = ZIO.effectTotal {
    given CanEqual[nio.ByteOrder | Null, Null] = CanEqual.derived
    val order = unsafeGetOrder()
    if order != null && order == nio.ByteOrder.LITTLE_ENDIAN.nn then ByteOrder.LittleEndian else ByteOrder.BigEndian
  }

  final def slice: UIO[Buffer[A]] = ZIO.effectTotal(unsafeFromNio(nioBuffer.slice().nn))

  final def duplicate: UIO[Buffer[A]] = ZIO.effectTotal(unsafeFromNio(nioBuffer.duplicate().nn))

  final def capacity: Int = nioBuffer.capacity

  final def position: UIO[Int] = ZIO.effectTotal(nioBuffer.position)

  final def position(newPosition: Int): UIO[Unit] = ZIO.effectTotal(nioBuffer.position(newPosition)).unit

  final def movePosition(delta: Int): UIO[Int] =
    for 
      pos <- position
      newPos = pos + delta
      _ <- position(newPos)
    yield newPos

  final def limit: UIO[Int] = ZIO.effectTotal(nioBuffer.limit)

  final def limit(newLimit: Int): UIO[Unit] = ZIO.effectTotal(nioBuffer.limit(newLimit)).unit

  final def moveLimit(delta: Int): UIO[Int] =
    for
      pos <- limit
      newPos = pos + delta
      _ <- limit(newPos)
    yield newPos

  final def remaining: UIO[Int] = ZIO.effectTotal(nioBuffer.remaining)

  final def hasRemaining: UIO[Boolean] = ZIO.effectTotal(nioBuffer.hasRemaining)

  final def mark: UIO[Unit] = ZIO.effectTotal(nioBuffer.mark()).unit

  final def reset: UIO[Unit] = ZIO.effectTotal(nioBuffer.reset()).unit

  final def clear: UIO[Unit] = ZIO.effectTotal(nioBuffer.clear()).unit

  final def flip: UIO[Unit] = ZIO.effectTotal(nioBuffer.flip()).unit

  final def rewind: UIO[Unit] = ZIO.effectTotal(nioBuffer.rewind()).unit

  final def isReadOnly: Boolean = nioBuffer.isReadOnly

  final def hasArray: Boolean = nioBuffer.hasArray

  final def isDirect: Boolean = nioBuffer.isDirect

  final def putChunk(chunk: Chunk[A])(using ClassTag[A]): UIO[Chunk[A]] =
    for
      r <- remaining
      (putChunk, remainderChunk) = chunk.splitAt(r)
      _ <- ZIO.effectTotal(unsafePutArray(putChunk.toArray))
    yield remainderChunk

  final def array: UIO[(Array[A], Int)] = ZIO.effectTotal(unsafeGetArray().nn -> nioBuffer.arrayOffset)

  final def withArray[R, E, B](noArray: ZIO[R, E, B], hasArray: (Array[A], Int) => ZIO[R, E, B]): ZIO[R, E, B] =
    if nioBuffer.hasArray then
      for
        (array, offset) <- this.array
        result <- hasArray(array, offset)
      yield result
    else
      noArray

  // *** Byte-specific API
  
  def alignedSlice(unitSize: Int)(using A =:= Byte): UIO[Buffer[Byte]] = dieNotByteBuffer

  def alignmentOffset(index: Int, unitSize: Int)(using A =:= Byte): UIO[Int] = dieNotByteBuffer

  def asCharBuffer(using A =:= Byte): UIO[Buffer[Char]] = dieNotByteBuffer

  def asDoubleBuffer(using A =:= Byte): UIO[Buffer[Double]] = dieNotByteBuffer

  def asFloatBuffer(using A =:= Byte): UIO[Buffer[Float]] = dieNotByteBuffer

  def asIntBuffer(using A =:= Byte): UIO[Buffer[Int]] = dieNotByteBuffer

  def asLongBuffer(using A =:= Byte): UIO[Buffer[Long]] = dieNotByteBuffer

  def asShortBuffer(using A =:= Byte): UIO[Buffer[Short]] = dieNotByteBuffer

  def getChar(using A =:= Byte): UIO[Char] = dieNotByteBuffer

  def getChar(index: Int)(using A =:= Byte): UIO[Char] = dieNotByteBuffer
  
  def getDouble(using A =:= Byte): UIO[Double] = dieNotByteBuffer

  def getDouble(index: Int)(using A =:= Byte): UIO[Double] = dieNotByteBuffer

  def getFloat(using A =:= Byte): UIO[Float] = dieNotByteBuffer

  def getFloat(index: Int)(using A =:= Byte): UIO[Float] = dieNotByteBuffer

  def getInt(using A =:= Byte): UIO[Int] = dieNotByteBuffer

  def getInt(index: Int)(using A =:= Byte): UIO[Int] = dieNotByteBuffer

  def getLong(using A =:= Byte): UIO[Long] = dieNotByteBuffer

  def getLong(index: Int)(using A =:= Byte): UIO[Long] = dieNotByteBuffer

  def getShort(using A =:= Byte): UIO[Short] = dieNotByteBuffer

  def getShort(index: Int)(using A =:= Byte): UIO[Short] = dieNotByteBuffer

  def putChar(value: Char)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putChar(index: Int, value: Char)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putDouble(value: Double)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putDouble(index: Int, value: Double)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putFloat(value: Float)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putFloat(index: Int, value: Float)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putInt(value: Int)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putInt(index: Int, value: Int)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putLong(value: Long)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putLong(index: Int, value: Long)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putShort(value: Short)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

  def putShort(index: Int, value: Short)(using A =:= Byte): UIO[Unit] = dieNotByteBuffer

end Buffer

object Buffer:
  
  private val dieNotByteBuffer = UIO.dieMessage("Not a byte buffer")

  object byte:
    def allocate(capacity: Int): UIO[Buffer[Byte]] = ZIO.effectTotal(new ByteBuffer(nio.ByteBuffer.allocate(capacity).nn))

    def allocateDirect(capacity: Int): UIO[Buffer[Byte]] = ZIO.effectTotal(new ByteBuffer(nio.ByteBuffer.allocateDirect(capacity).nn))

    def fromChunk(chunk: Chunk[Byte]): UIO[Buffer[Byte]] = allocate(chunk.size).tap(_.putChunk(chunk)).tap(_.clear)
    
  object char:
    def allocate(capacity: Int): UIO[Buffer[Char]] = ZIO.effectTotal(new CharBuffer(nio.CharBuffer.allocate(capacity).nn))

    def fromChunk(chunk: Chunk[Char]): UIO[Buffer[Char]] = allocate(chunk.size).tap(_.putChunk(chunk)).tap(_.clear)

  object double:
    def allocate(capacity: Int): UIO[Buffer[Double]] = ZIO.effectTotal(new DoubleBuffer(nio.DoubleBuffer.allocate(capacity).nn))

    def fromChunk(chunk: Chunk[Double]): UIO[Buffer[Double]] = allocate(chunk.size).tap(_.putChunk(chunk)).tap(_.clear)

  object float:
    def allocate(capacity: Int): UIO[Buffer[Float]] = ZIO.effectTotal(new FloatBuffer(nio.FloatBuffer.allocate(capacity).nn))

    def fromChunk(chunk: Chunk[Float]): UIO[Buffer[Float]] = allocate(chunk.size).tap(_.putChunk(chunk)).tap(_.clear)

  object int:
    def allocate(capacity: Int): UIO[Buffer[Int]] = ZIO.effectTotal(new IntBuffer(nio.IntBuffer.allocate(capacity).nn))

    def fromChunk(chunk: Chunk[Int]): UIO[Buffer[Int]] = allocate(chunk.size).tap(_.putChunk(chunk)).tap(_.clear)

  object long:
    def allocate(capacity: Int): UIO[Buffer[Long]] = ZIO.effectTotal(new LongBuffer(nio.LongBuffer.allocate(capacity).nn))

    def fromChunk(chunk: Chunk[Long]): UIO[Buffer[Long]] = allocate(chunk.size).tap(_.putChunk(chunk)).tap(_.clear)

  object short:
    def allocate(capacity: Int): UIO[Buffer[Short]] = ZIO.effectTotal(new ShortBuffer(nio.ShortBuffer.allocate(capacity).nn))

    def fromChunk(chunk: Chunk[Short]): UIO[Buffer[Short]] = allocate(chunk.size).tap(_.putChunk(chunk)).tap(_.clear)

end Buffer

extension (b: Buffer[Byte])
  @targetName("useJavaByte")
  def useJava[R, E, A](f: nio.ByteBuffer => ZIO[R, E, A]): ZIO[R, E, A] = f(b.nioBuffer.asInstanceOf[nio.ByteBuffer])

extension (b: Buffer[Char])
  @targetName("useJavaChar")
  def useJava[R, E, A](f: nio.CharBuffer => ZIO[R, E, A]): ZIO[R, E, A] = f(b.nioBuffer.asInstanceOf[nio.CharBuffer])

extension (b: Buffer[Double])
  @targetName("useJavaDouble")
  def useJava[R, E, A](f: nio.DoubleBuffer => ZIO[R, E, A]): ZIO[R, E, A] = f(b.nioBuffer.asInstanceOf[nio.DoubleBuffer])

extension (b: Buffer[Float])
  @targetName("useJavaFloat")
  def useJava[R, E, A](f: nio.FloatBuffer => ZIO[R, E, A]): ZIO[R, E, A] = f(b.nioBuffer.asInstanceOf[nio.FloatBuffer])

extension (b: Buffer[Int])
  @targetName("useJavaInt")
  def useJava[R, E, A](f: nio.IntBuffer => ZIO[R, E, A]): ZIO[R, E, A] = f(b.nioBuffer.asInstanceOf[nio.IntBuffer])

extension (b: Buffer[Long])
  @targetName("useJavaLong")
  def useJava[R, E, A](f: nio.LongBuffer => ZIO[R, E, A]): ZIO[R, E, A] = f(b.nioBuffer.asInstanceOf[nio.LongBuffer])

extension (b: Buffer[Short])
  @targetName("useJavaShort")
  def useJava[R, E, A](f: nio.ShortBuffer => ZIO[R, E, A]): ZIO[R, E, A] = f(b.nioBuffer.asInstanceOf[nio.ShortBuffer])


private class ByteBuffer private[buffers] (protected[buffers] val nioBuffer: nio.ByteBuffer) extends Buffer[Byte]:

  override protected final def unsafeGetArray(): Array[Byte] | Null = nioBuffer.array

  override protected final def unsafePutArray(a: Array[Byte]): Unit = nioBuffer.put(a)

  override protected final def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[Byte] = new ByteBuffer(nioBuffer.asInstanceOf[nio.ByteBuffer])

  override protected final def unsafeGetOrder(): nio.ByteOrder | Null = nioBuffer.order

  override protected final def unsafeCompact(): Any = nioBuffer.compact()

  override protected final def unsafeAsReadOnlyBuffer(): nio.ByteBuffer | Null = nioBuffer.asReadOnlyBuffer

  override protected final def unsafeGet(): Byte = nioBuffer.get()

  override protected final def unsafeGet(index: Int): Byte = nioBuffer.get(index)

  override protected final def unsafeGet(a: Array[Byte]): Any = nioBuffer.get(a)

  override protected final def unsafePut(element: Byte): Any = nioBuffer.put(element)

  override protected final def unsafePut(index: Int, element: Byte): Any = nioBuffer.put(index, element)

  override protected final def unsafePutBuffer(src: Buffer[Byte]): Any = nioBuffer.put(src.asInstanceOf[ByteBuffer].nioBuffer)

  override protected final def unsafeMismatch(other: Buffer[Byte]): Int = nioBuffer.mismatch(other.asInstanceOf[ByteBuffer].nioBuffer)

  // *** Byte-specific API
  
  override final def alignedSlice(unitSize: Int)(using Byte =:= Byte): UIO[Buffer[Byte]] = ZIO.effectTotal(new ByteBuffer(nioBuffer.alignedSlice(unitSize).nn))

  override final def alignmentOffset(index: Int, unitSize: Int)(using Byte =:= Byte): UIO[Int] = ZIO.effectTotal(nioBuffer.alignmentOffset(index, unitSize))

  override final def asCharBuffer(using Byte =:= Byte): UIO[Buffer[Char]] = ZIO.effectTotal(new CharBuffer(nioBuffer.asCharBuffer().nn))

  override def asDoubleBuffer(using Byte =:= Byte): UIO[Buffer[Double]] = ZIO.effectTotal(new DoubleBuffer(nioBuffer.asDoubleBuffer().nn))

  override def asFloatBuffer(using Byte =:= Byte): UIO[Buffer[Float]] = ZIO.effectTotal(new FloatBuffer(nioBuffer.asFloatBuffer().nn))

  override def asIntBuffer(using Byte =:= Byte): UIO[Buffer[Int]] = ZIO.effectTotal(new IntBuffer(nioBuffer.asIntBuffer().nn))

  override def asLongBuffer(using Byte =:= Byte): UIO[Buffer[Long]] = ZIO.effectTotal(new LongBuffer(nioBuffer.asLongBuffer().nn))

  override def asShortBuffer(using Byte =:= Byte): UIO[Buffer[Short]] = ZIO.effectTotal(new ShortBuffer(nioBuffer.asShortBuffer().nn))

  override def getChar(using Byte =:= Byte): UIO[Char] = ZIO.effectTotal(nioBuffer.getChar())

  override def getChar(index: Int)(using Byte =:= Byte): UIO[Char] = ZIO.effectTotal(nioBuffer.getChar(index))

  override def getDouble(using Byte =:= Byte): UIO[Double] = ZIO.effectTotal(nioBuffer.getDouble())

  override def getDouble(index: Int)(using Byte =:= Byte): UIO[Double] = ZIO.effectTotal(nioBuffer.getDouble(index))

  override def getFloat(using Byte =:= Byte): UIO[Float] = ZIO.effectTotal(nioBuffer.getFloat())

  override def getFloat(index: Int)(using Byte =:= Byte): UIO[Float] = ZIO.effectTotal(nioBuffer.getFloat(index))

  override def getInt(using Byte =:= Byte): UIO[Int] = ZIO.effectTotal(nioBuffer.getInt())

  override def getInt(index: Int)(using Byte =:= Byte): UIO[Int] = ZIO.effectTotal(nioBuffer.getInt(index))

  override def getLong(using Byte =:= Byte): UIO[Long] = ZIO.effectTotal(nioBuffer.getLong())

  override def getLong(index: Int)(using Byte =:= Byte): UIO[Long] = ZIO.effectTotal(nioBuffer.getLong(index))

  override def getShort(using Byte =:= Byte): UIO[Short] = ZIO.effectTotal(nioBuffer.getShort())

  override def getShort(index: Int)(using Byte =:= Byte): UIO[Short] = ZIO.effectTotal(nioBuffer.getShort(index))

  override def putChar(value: Char)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putChar(value)).unit

  override def putChar(index: Int, value: Char)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putChar(index, value)).unit

  override def putDouble(value: Double)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putDouble(value)).unit

  override def putDouble(index: Int, value: Double)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putDouble(index, value)).unit

  override def putFloat(value: Float)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putFloat(value)).unit

  override def putFloat(index: Int, value: Float)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putFloat(index, value)).unit

  override def putInt(value: Int)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putInt(value)).unit

  override def putInt(index: Int, value: Int)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putInt(index, value)).unit

  override def putLong(value: Long)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putLong(value)).unit

  override def putLong(index: Int, value: Long)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putLong(index, value)).unit

  override def putShort(value: Short)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putShort(value)).unit

  override def putShort(index: Int, value: Short)(using Byte =:= Byte): UIO[Unit] = ZIO.effectTotal(nioBuffer.putShort(index, value)).unit

end ByteBuffer

final class CharBuffer private[buffers] (protected[buffers] val nioBuffer: nio.CharBuffer) extends Buffer[Char]:

  override protected def unsafeGetArray(): Array[Char] | Null = nioBuffer.array

  override protected def unsafePutArray(a: Array[Char]): Unit = nioBuffer.put(a)

  override protected def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[Char] = new CharBuffer(nioBuffer.asInstanceOf[nio.CharBuffer])

  override protected def unsafeGetOrder(): nio.ByteOrder | Null = nioBuffer.order

  override protected def unsafeCompact(): Any = nioBuffer.compact()

  override protected def unsafeAsReadOnlyBuffer(): nio.CharBuffer | Null = nioBuffer.asReadOnlyBuffer

  override protected def unsafeGet(): Char = nioBuffer.get()

  override protected def unsafeGet(index: Int): Char = nioBuffer.get(index)

  override protected def unsafeGet(a: Array[Char]): Any = nioBuffer.get(a)

  override protected def unsafePut(element: Char): Any = nioBuffer.put(element)

  override protected def unsafePut(index: Int, element: Char): Any = nioBuffer.put(index, element)

  override protected def unsafePutBuffer(src: Buffer[Char]): Any = nioBuffer.put(src.asInstanceOf[CharBuffer].nioBuffer)

  override protected def unsafeMismatch(other: Buffer[Char]): Int = nioBuffer.mismatch(other.asInstanceOf[CharBuffer].nioBuffer)

end CharBuffer

final class DoubleBuffer private[buffers] (protected[buffers] val nioBuffer: nio.DoubleBuffer) extends Buffer[Double]:

  override protected def unsafeGetArray(): Array[Double] | Null = nioBuffer.array

  override protected def unsafePutArray(a: Array[Double]): Unit = nioBuffer.put(a)

  override protected def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[Double] = new DoubleBuffer(nioBuffer.asInstanceOf[nio.DoubleBuffer])

  override protected def unsafeGetOrder(): nio.ByteOrder | Null = nioBuffer.order

  override protected def unsafeCompact(): Any = nioBuffer.compact()

  override protected def unsafeAsReadOnlyBuffer(): nio.DoubleBuffer | Null = nioBuffer.asReadOnlyBuffer

  override protected def unsafeGet(): Double = nioBuffer.get()

  override protected def unsafeGet(index: Int): Double = nioBuffer.get(index)

  override protected def unsafeGet(a: Array[Double]): Any = nioBuffer.get(a)

  override protected def unsafePut(element: Double): Any = nioBuffer.put(element)

  override protected def unsafePut(index: Int, element: Double): Any = nioBuffer.put(index, element)

  override protected def unsafePutBuffer(src: Buffer[Double]): Any = nioBuffer.put(src.asInstanceOf[DoubleBuffer].nioBuffer)

  override protected def unsafeMismatch(other: Buffer[Double]): Int = nioBuffer.mismatch(other.asInstanceOf[DoubleBuffer].nioBuffer)

end DoubleBuffer

final class FloatBuffer private[buffers] (protected[buffers] val nioBuffer: nio.FloatBuffer) extends Buffer[Float]:

  override protected def unsafeGetArray(): Array[Float] | Null = nioBuffer.array

  override protected def unsafePutArray(a: Array[Float]): Unit = nioBuffer.put(a)

  override protected def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[Float] = new FloatBuffer(nioBuffer.asInstanceOf[nio.FloatBuffer])

  override protected def unsafeGetOrder(): nio.ByteOrder | Null = nioBuffer.order

  override protected def unsafeCompact(): Any = nioBuffer.compact()

  override protected def unsafeAsReadOnlyBuffer(): nio.FloatBuffer | Null = nioBuffer.asReadOnlyBuffer

  override protected def unsafeGet(): Float = nioBuffer.get()

  override protected def unsafeGet(index: Int): Float = nioBuffer.get(index)

  override protected def unsafeGet(a: Array[Float]): Any = nioBuffer.get(a)

  override protected def unsafePut(element: Float): Any = nioBuffer.put(element)

  override protected def unsafePut(index: Int, element: Float): Any = nioBuffer.put(index, element)

  override protected def unsafePutBuffer(src: Buffer[Float]): Any = nioBuffer.put(src.asInstanceOf[FloatBuffer].nioBuffer)

  override protected def unsafeMismatch(other: Buffer[Float]): Int = nioBuffer.mismatch(other.asInstanceOf[FloatBuffer].nioBuffer)

end FloatBuffer

final class IntBuffer private[buffers] (protected[buffers] val nioBuffer: nio.IntBuffer) extends Buffer[Int]:

  override protected def unsafeGetArray(): Array[Int] | Null = nioBuffer.array

  override protected def unsafePutArray(a: Array[Int]): Unit = nioBuffer.put(a)

  override protected def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[Int] = new IntBuffer(nioBuffer.asInstanceOf[nio.IntBuffer])

  override protected def unsafeGetOrder(): nio.ByteOrder | Null = nioBuffer.order

  override protected def unsafeCompact(): Any = nioBuffer.compact()

  override protected def unsafeAsReadOnlyBuffer(): nio.IntBuffer | Null = nioBuffer.asReadOnlyBuffer

  override protected def unsafeGet(): Int = nioBuffer.get()

  override protected def unsafeGet(index: Int): Int = nioBuffer.get(index)

  override protected def unsafeGet(a: Array[Int]): Any = nioBuffer.get(a)

  override protected def unsafePut(element: Int): Any = nioBuffer.put(element)

  override protected def unsafePut(index: Int, element: Int): Any = nioBuffer.put(index, element)

  override protected def unsafePutBuffer(src: Buffer[Int]): Any = nioBuffer.put(src.asInstanceOf[IntBuffer].nioBuffer)

  override protected def unsafeMismatch(other: Buffer[Int]): Int = nioBuffer.mismatch(other.asInstanceOf[IntBuffer].nioBuffer)

end IntBuffer

final class LongBuffer private[buffers] (protected[buffers] val nioBuffer: nio.LongBuffer) extends Buffer[Long]:

  override protected def unsafeGetArray(): Array[Long] | Null = nioBuffer.array

  override protected def unsafePutArray(a: Array[Long]): Unit = nioBuffer.put(a)

  override protected def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[Long] = new LongBuffer(nioBuffer.asInstanceOf[nio.LongBuffer])

  override protected def unsafeGetOrder(): nio.ByteOrder | Null = nioBuffer.order

  override protected def unsafeCompact(): Any = nioBuffer.compact()

  override protected def unsafeAsReadOnlyBuffer(): nio.LongBuffer | Null = nioBuffer.asReadOnlyBuffer

  override protected def unsafeGet(): Long = nioBuffer.get()

  override protected def unsafeGet(index: Int): Long = nioBuffer.get(index)

  override protected def unsafeGet(a: Array[Long]): Any = nioBuffer.get(a)

  override protected def unsafePut(element: Long): Any = nioBuffer.put(element)

  override protected def unsafePut(index: Int, element: Long): Any = nioBuffer.put(index, element)

  override protected def unsafePutBuffer(src: Buffer[Long]): Any = nioBuffer.put(src.asInstanceOf[LongBuffer].nioBuffer)

  override protected def unsafeMismatch(other: Buffer[Long]): Int = nioBuffer.mismatch(other.asInstanceOf[LongBuffer].nioBuffer)

end LongBuffer

final private[buffers] class ShortBuffer (protected[buffers] val nioBuffer: nio.ShortBuffer) extends Buffer[Short]:

  override protected def unsafeGetArray(): Array[Short] | Null = nioBuffer.array

  override protected def unsafePutArray(a: Array[Short]): Unit = nioBuffer.put(a)

  override protected def unsafeFromNio(nioBuffer: nio.Buffer): Buffer[Short] = new ShortBuffer(nioBuffer.asInstanceOf[nio.ShortBuffer])

  override protected def unsafeGetOrder(): nio.ByteOrder | Null = nioBuffer.order

  override protected def unsafeCompact(): Any = nioBuffer.compact()

  override protected def unsafeAsReadOnlyBuffer(): nio.ShortBuffer | Null = nioBuffer.asReadOnlyBuffer

  override protected def unsafeGet(): Short = nioBuffer.get()

  override protected def unsafeGet(index: Int): Short = nioBuffer.get(index)

  override protected def unsafeGet(a: Array[Short]): Any = nioBuffer.get(a)

  override protected def unsafePut(element: Short): Any = nioBuffer.put(element)

  override protected def unsafePut(index: Int, element: Short): Any = nioBuffer.put(index, element)

  override protected def unsafePutBuffer(src: Buffer[Short]): Any = nioBuffer.put(src.asInstanceOf[ShortBuffer].nioBuffer)

  override protected def unsafeMismatch(other: Buffer[Short]): Int = nioBuffer.mismatch(other.asInstanceOf[ShortBuffer].nioBuffer)

end ShortBuffer

final class MappedByteBuffer private[buffers] (nioMappedBuffer: nio.MappedByteBuffer) extends ByteBuffer(nioMappedBuffer):

  def isLoaded: UIO[Boolean] = ZIO.effectTotal(nioMappedBuffer.isLoaded())

  def load: URIO[Blocking, Unit] = blocking.effectBlocking(nioMappedBuffer.load()).orDie.unit

  def force: URIO[Blocking, Unit] = blocking.effectBlocking(nioMappedBuffer.force()).orDie.unit

end MappedByteBuffer
