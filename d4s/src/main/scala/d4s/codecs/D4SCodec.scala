package d4s.codecs

import java.util

import d4s.codecs.D4SCodec.Imap2PartiallyApplied
import d4s.models.DynamoException.DecoderException
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait D4SCodec[A] extends D4SAttributeCodec[A] with D4SEncoder[A] {
  override final def imap[B](decode: A => B)(encode: B => A): D4SCodec[B] = {
    D4SCodec.fromPair(contramap(encode), map(decode))
  }
  final def imap2[B, C](another: D4SCodec[B])(decode: (A, B) => C)(encode: C => (A, B)): D4SCodec[C] = {
    D4SCodec.fromPair(contramap2(another)(encode), map2(another)(decode))
  }
  final def imap2[B, C](anotherEnc: D4SEncoder[B], anotherDec: D4SDecoder[B])(decode: (A, B) => C)(encode: C => (A, B)): D4SCodec[C] = {
    D4SCodec.fromPair(contramap2(anotherEnc)(encode), map2(anotherDec)(decode))
  }
  final def imap2[B]: Imap2PartiallyApplied[A, B] = new Imap2PartiallyApplied[A, B](this)
  final def imapObject(
    decode: Map[String, AttributeValue] => Map[String, AttributeValue]
  )(encode: Map[String, AttributeValue] => Map[String, AttributeValue]): D4SCodec[A] = {
    D4SCodec.fromPair(mapObject(decode), contramapObject(encode))
  }
  override final def appendFields[Item: D4SEncoder](f: (A, Map[String, AttributeValue]) => Item): D4SCodec[A] = {
    D4SCodec.fromPair(super.appendFields(f), this)
  }
}

object D4SCodec {
  @inline def apply[A: D4SCodec]: D4SCodec[A]                            = implicitly
  def derived[A](implicit derivedCodec: D4SDerivedCodec[A]): D4SCodec[A] = D4SCodec.fromPair(derivedCodec.enc, derivedCodec.dec)

  def from[T: D4SEncoder: D4SDecoder]: D4SCodec[T] = fromPair(D4SEncoder[T], D4SDecoder[T])
  def fromPair[T](encoder: D4SEncoder[T], decoder: D4SDecoder[T]): D4SCodec[T] = new D4SCodec[T] {
    override def encode(item: T): Map[String, AttributeValue]          = encoder.encode(item)
    override def encodeJava(item: T): util.Map[String, AttributeValue] = encoder.encodeJava(item)

    override def decode(item: Map[String, AttributeValue]): Either[DecoderException, T]      = decoder.decode(item)
    override def decode(item: util.Map[String, AttributeValue]): Either[DecoderException, T] = decoder.decode(item)
    override def decodeAttribute(attr: AttributeValue): Either[DecoderException, T]          = decoder.decodeAttribute(attr)
  }

  @deprecated("Use derived", "1.0.3")
  def derive[A](implicit derivedCodec: D4SDerivedCodec[A]): D4SCodec[A] = D4SCodec.fromPair(derivedCodec.enc, derivedCodec.dec)

  final class Imap2PartiallyApplied[A, B](private val self: D4SCodec[A]) extends AnyVal {
    def apply[C](decode: (A, B) => C)(encode: C => (A, B))(implicit enc: D4SEncoder[B], dec: D4SDecoder[B]): D4SCodec[C] = {
      self.imap2(enc, dec)(decode)(encode)
    }
  }
}
