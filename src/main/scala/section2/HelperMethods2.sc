import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}


trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}

object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)
    Using(new FileOutputStream("/home/phil/PDev/udemy/fp-with-scala-cats/obj")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

case class Switch(isOn: Boolean)

object Switch {
  implicit object SwitchByteEncoder extends ByteEncoder[Switch] {
    override def encode(s: Switch): Array[Byte] = {
      Array(if (s.isOn) '1'.toByte else '0'.toByte)
    }
  }
}

FileChannel.write[Switch](Switch(true))

// Common situation:
// For each of type A:
// - 1 main instance of ByteEncoder
// - couple more instances for specific use cases

// Goal:
// - Use the main instance by default
// - Provide a different instance for specific use cases