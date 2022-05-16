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

//implicit object OptionString extends ByteEncoder[Option[String]] {
//  override def encode(a: Option[String]): Array[Byte] = a match {
//    case None => Array.emptyByteArray
//    case Some(s) => StringByteEncoder.encode(s)
//  }
//}
//
//implicit object OptionInt extends ByteEncoder[Option[Int]] {
//  override def encode(a: Option[Int]): Array[Byte] = a match {
//    case None => Array.emptyByteArray
//    case Some(i) => IntByteEncoder.encode(i)
//  }
//}

implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = new ByteEncoder[Option[A]] {
  override def encode(a: Option[A]): Array[Byte] = a match {
    case None => Array.emptyByteArray
    case Some(a) => encA.encode(a)
  }
}


ByteEncoder[String].encode("hello")
ByteEncoder[Int].encode(1000)
ByteEncoder[Option[String]].encode(Option("world"))
ByteEncoder[Option[String]].encode(None)
ByteEncoder[Option[Int]].encode(Option(1000))
ByteEncoder[Option[Int]].encode(None)