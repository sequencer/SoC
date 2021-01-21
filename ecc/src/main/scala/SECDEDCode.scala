package org.chipsalliance.utils.ecc

import chisel3._
import chisel3.util.Cat

class SECDEDCode extends Code {
  def canDetect = true
  def canCorrect = true

  private val sec = new SECCode
  private val par = new ParityCode

  def width(k: Int) = sec.width(k) + 1
  def eccIndices(w0: Int) = {
    (0 until width(w0)).collect {
      case i if i >= w0 => i
    }
  }
  def encode(x: UInt, poison: Bool = false.B) = {
    // toggling two bits ensures the error is uncorrectable
    // to ensure corrected == uncorrected, we pick one redundant
    // bit from SEC (the highest); correcting it does not affect
    // corrected == uncorrected. the second toggled bit is the
    // parity bit, which also does not appear in the decoding
    val toggle_lo = Cat(poison.asUInt, poison.asUInt)
    val toggle_hi = toggle_lo << (sec.width(x.getWidth) - 1)
    par.encode(sec.encode(x)) ^ toggle_hi
  }
  def swizzle(x: UInt) = par.swizzle(sec.swizzle(x))
  def decode(x:  UInt) = new Decoding {
    val secdec = sec.decode(x(x.getWidth - 2, 0))
    val pardec = par.decode(x)

    val uncorrected = secdec.uncorrected
    val corrected = secdec.corrected
    val correctable = pardec.uncorrectable
    val uncorrectable = !pardec.uncorrectable && secdec.correctable
  }
}
