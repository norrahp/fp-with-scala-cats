import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

val a: Int = 5

trait Channel {
  def write(obj: Any): Unit
}

object FileChannel extends Channel {
  override def write(obj: Any): Unit = {
    val bytes: Array[Byte] = obj match {
      case n: Int =>
        val bb = ByteBuffer.allocate(4)
        bb.putInt(n)
        bb.array()

      case s: String =>
        s.getBytes

      case invalid => throw new Exception("unhandled")
    }

    Using(new FileOutputStream("/home/phil/PDev/udemy/fp-with-scala-cats/test")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

FileChannel.write("hello")