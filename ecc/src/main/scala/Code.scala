package org.chipsalliance.utils.ecc

import chisel3._

abstract class Code {

  /** can error being detected by this code? */
  def canDetect: Boolean

  /** can error being corrected by this code? */
  def canCorrect: Boolean

  /** Final length of encoded data.
    *
    * @param w0 unencoded width.
    */
  def width(w0: Int): Int

  /** Takes the unencoded width and returns a list of indices indicating which
    * bits of the encoded value will be used for ecc
    *
    * @param width unencoded width
    */
  def eccIndices(width: Int): Seq[Int]

  /** Encode x to a codeword suitable for decode.
    * If poison is true, the decoded value will report uncorrectable
    * error despite uncorrected == corrected == x.
    *
    * @param x data to be encoded.
    * @param poison poison this encoding.
    */
  def encode(x: UInt, poison: Bool = false.B): UInt

  /** Decode result from payload `x`.
    *
    * @param x to be decoded.
    */
  def decode(x: UInt): Decoding

  /** Copy the bits in `x` to the right bit positions in an encoded word,
    * so that x === decode(swizzle(x)).uncorrected; but don't generate
    * the other code bits, so decode(swizzle(x)).error might be true.
    * For codes for which this operation is not trivial, throw an
    * UnsupportedOperationException.
    */
  def swizzle(x: UInt): UInt
}

object Code {
  def fromString(s: Option[String]): Code = fromString(s.getOrElse("none"))
  def fromString(s: String): Code = s.toLowerCase match {
    case "none"     => new IdentityCode
    case "identity" => new IdentityCode
    case "parity"   => new ParityCode
    case "sec"      => new SECCode
    case "secded"   => new SECDEDCode
    case _          => throw new IllegalArgumentException("Unknown ECC type")
  }
}
