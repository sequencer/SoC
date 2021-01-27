package org.chipsalliance.utils.decoder.qmc

import chisel3._
import chisel3.util.{BitPat, Cat}

import scala.collection.mutable.{ArrayBuffer, Map}

/** Quine–McCluskey decoder. */
object DecodeLogic {
  def term(lit: BitPat) = new Term(lit.value, BigInt(2).pow(lit.getWidth) - (lit.mask + 1))

  def logic(addr: UInt, addrWidth: Int, cache: Map[Term, Bool], terms: Seq[Term]): Bool = {
    terms.map { t =>
      cache
        .getOrElseUpdate(
          t,
          (
            if (t.mask == 0) addr
            else addr & (BigInt(2).pow(addrWidth) - (t.mask + 1)).U(addrWidth.W)
          ) === t.value.U(addrWidth.W)
        )
    }
      .foldLeft(false.B)(_ || _)
  }

  def apply(addr: UInt, default: BitPat, mapping: Iterable[(BitPat, BitPat)]): UInt = {
    val cache = caches.getOrElseUpdate(addr, Map[Term, Bool]())
    val defaultTerm = term(default)
    val (keys, values) = mapping.unzip
    val addrWidth = keys.map(_.getWidth).max
    val terms = keys.toList.map(k => term(k))
    val termvalues = terms.zip(values.toList.map(term))

    for (t <- keys.zip(terms).tails; if t.nonEmpty)
      for (u <- t.tail)
        assert(
          !t.head._2.intersects(u._2),
          "DecodeLogic: keys " + t.head + " and " + u + " overlap"
        )

    Cat(
      (0 until default.getWidth.max(values.map(_.getWidth).max))
        .map({ i: Int =>
          val mint =
            termvalues.filter { case (_, t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 1 }.map(_._1)
          val maxt =
            termvalues.filter { case (_, t) => ((t.mask >> i) & 1) == 0 && ((t.value >> i) & 1) == 0 }.map(_._1)
          val dc = termvalues.filter { case (_, t) => ((t.mask >> i) & 1) == 1 }.map(_._1)
          if (((defaultTerm.mask >> i) & 1) != 0) {
            logic(addr, addrWidth, cache, SimplifyDC(mint, maxt, addrWidth))
          } else {
            val defbit = (defaultTerm.value.toInt >> i) & 1
            val t = if (defbit == 0) mint else maxt
            val bit = logic(addr, addrWidth, cache, Simplify(t, dc, addrWidth))
            if (defbit == 0) bit else ~bit
          }
        })
        .reverse
    )
  }

  def apply(addr: UInt, default: Seq[BitPat], mappingIn: Iterable[(BitPat, Seq[BitPat])]): Seq[UInt] = {
    val mapping = ArrayBuffer.fill(default.size)(ArrayBuffer[(BitPat, BitPat)]())
    for ((key, values) <- mappingIn)
      for ((value, i) <- values.zipWithIndex)
        mapping(i) += key -> value
    for ((thisDefault, thisMapping) <- default.zip(mapping))
      yield apply(addr, thisDefault, thisMapping)
  }

  def apply(addr: UInt, default: Seq[BitPat], mappingIn: List[(UInt, Seq[BitPat])]): Seq[UInt] =
    apply(addr, default, mappingIn.map(m => (BitPat(m._1), m._2)).asInstanceOf[Iterable[(BitPat, Seq[BitPat])]])

  def apply(addr: UInt, trues: Iterable[UInt], falses: Iterable[UInt]): Bool =
    apply(
      addr,
      BitPat.dontCare(1),
      trues.map(BitPat(_) -> BitPat("b1")) ++ falses.map(BitPat(_) -> BitPat("b0"))
    ).asBool

  /** decoder cache during a chisel elaboration. */
  private val caches = Map[UInt, Map[Term, Bool]]()
}
