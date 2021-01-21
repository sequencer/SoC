package org.chipsalliance.utils.ecc

import chisel3._
import chisel3.util.{isPow2, log2Ceil, log2Floor, Cat, UIntToOH}
class SECCode extends Code {
  def canDetect = true
  def canCorrect = true

  // SEC codes may or may not be poisonous depending on the length
  // If the code is perfect, every non-codeword is correctable
  def poisonous(n: Int) = !isPow2(n + 1)

  def width(k: Int): Int = {
    val m = log2Floor(k) + 1
    k + m + (if ((1 << m) < m + k + 1) 1 else 0)
  }

  def eccIndices(w0: Int) = {
    (0 until width(w0)).collect {
      case i if i >= w0 => i
    }
  }

  def swizzle(x: UInt) = {
    val k = x.getWidth
    val n = width(k)
    Cat(0.U((n - k).W), x)
  }

  // An (n=16, k=11) Hamming code is naturally encoded as:
  //   PPxPxxxPxxxxxxxP where P are parity bits and x are data
  //   Indexes typically start at 1, because then the P are on powers of two
  // In systematic coding, you put all the data in the front:
  //   xxxxxxxxxxxPPPPP
  //   Indexes typically start at 0, because Computer Science
  // For sanity when reading SRAMs, you want systematic form.

  private def impl(n: Int, k: Int) = {
    require(n >= 3 && k >= 1 && !isPow2(n))
    val hamm2sys = IndexedSeq.tabulate(n + 1) { i =>
      if (i == 0) {
        n /* undefined */
      } else if (isPow2(i)) {
        k + log2Ceil(i)
      } else {
        i - 1 - log2Ceil(i)
      }
    }
    val sys2hamm = hamm2sys.zipWithIndex.sortBy(_._1).map(_._2).toIndexedSeq
    def syndrome(j: Int): UInt = {
      val bit = 1 << j
      ("b" + Seq
        .tabulate(n) { i =>
          if ((sys2hamm(i) & bit) != 0) "1" else "0"
        }
        .reverse).mkString.U
    }
    (hamm2sys, sys2hamm, syndrome _)
  }

  def encode(x: UInt, poison: Bool = false.B) = {
    val k = x.getWidth
    val n = width(k)
    val (_, _, syndrome) = impl(n, k)

    require((poison.isLit && poison.litValue == 0) || poisonous(n), s"SEC code of length ${n} cannot be poisoned")

    /* By setting the entire syndrome on poison, the corrected bit falls off the end of the code */
    val syndromeUInt = VecInit.tabulate(n - k) { j => (syndrome(j)(k - 1, 0) & x).xorR ^ poison }.asUInt
    Cat(syndromeUInt, x)
  }

  def decode(y: UInt) = new Decoding {
    val n = y.getWidth
    val k = n - log2Ceil(n)
    val (_, sys2hamm, syndrome) = impl(n, k)

    val syndromeUInt = VecInit.tabulate(n - k) { j => (syndrome(j) & y).xorR }.asUInt

    val hammBadBitOH = UIntToOH(syndromeUInt, n + 1)
    val sysBadBitOH = VecInit.tabulate(k) { i => hammBadBitOH(sys2hamm(i)) }.asUInt

    val uncorrected = y(k - 1, 0)
    val corrected = uncorrected ^ sysBadBitOH
    val correctable = syndromeUInt.orR
    val uncorrectable = if (poisonous(n)) { syndromeUInt > n.U }
    else { false.B }
  }
}
