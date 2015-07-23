package scimap

import scala.collection.immutable.TreeSet
import scala.collection.immutable.NumericRange
import scala.math.ScalaNumber
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import java.security.MessageDigest
import java.util.Base64

trait Util {
  protected def threadLocalMd5Instance: MessageDigest
  
  def md5(bytes: Array[Byte]) = threadLocalMd5Instance.digest(bytes)
  
  def base64Decode(value: String) = Base64.getDecoder.decode(value)
  
  def base64Encode(bytes: Array[Byte]) = Base64.getEncoder.encodeToString(bytes)
}
object Util extends Util {
  lazy val threadLocalMd5 = new ThreadLocal[MessageDigest] {
    override protected def initialValue(): MessageDigest = MessageDigest.getInstance("MD5")
  }
  
  override protected def threadLocalMd5Instance: MessageDigest = threadLocalMd5.get
  
  object Implicits {
    implicit class StringInterpolations(val sc: StringContext) extends AnyVal {
      // Use s() for escapes, ref: https://issues.scala-lang.org/browse/SI-6476
      def ci = new CaseInsensitiveExtract(sc.s())
    }
  }
  
  class CaseInsensitiveExtract(val string: String) extends AnyVal {
    def unapply(other: String) = string.equalsIgnoreCase(other)
  }
}