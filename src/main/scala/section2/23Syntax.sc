import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = {
    s.getBytes
  }
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(i: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(i)
    bb.array()
  }
}

implicit class ByteEncoderOps[A](a: A) {
  def encode(implicit enc: ByteEncoder[A]): Array[Byte] =
    enc.encode(a)
}

5.encode
"hello world".encode


import scala.util.Try

trait ByteDecoder[A] {
  def decode(a: Array[Byte]): Option[A]
}

object ByteDecoder {
  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
    override def decode(bytes: Array[Byte]): Option[A] = f(bytes)
  }

  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev
}

//implicit object StringByteDecoder extends ByteDecoder[String] {
//  override def decode(bytes: Array[Byte]): Option[String] =
//    Try(new String(bytes)).toOption
//}

implicit object IntByteDecoder extends ByteDecoder[Int] {
  override def decode(bytes: Array[Byte]): Option[Int] =
    if (bytes.length != 4) None
    else {
      val bb = ByteBuffer.allocate(4)
      bb.put(bytes)
      bb.flip()
      Some(bb.getInt)
    }
}


implicit class ByteDecoderOps[A](bytes: Array[Byte]) {
  def decode(implicit dec: ByteDecoder[A]): Option[A] =
    dec.decode(bytes)
}

Array[Byte](0, 0, 0, 5).decode
Array[Byte](104, 101, 108, 108).decode
