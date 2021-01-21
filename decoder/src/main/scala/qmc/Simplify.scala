package org.chipsalliance.utils.decoder.qmc

import chisel3._

import scala.annotation.tailrec

object Simplify {
  def getPrimeImplicants(implicants: Seq[Term], bits: Int): Seq[Term] = {
    var prime = List[Term]()
    implicants.foreach(_.prime = true)
    val cols = (0 to bits).map(b => implicants.filter(b == _.mask.bitCount))
    val table = cols.map(c => (0 to bits).map(b => collection.mutable.Set(c.filter(b == _.value.bitCount): _*)))
    for (i <- 0 to bits) {
      for (j <- 0 until bits - i)
        table(i)(j).foreach(a =>
          table(i + 1)(j) ++=
            table(i)(j + 1)
              .filter(_.similar(a))
              .map(_.merge(a))
        )
      for (r <- table(i))
        for (p <- r; if p.prime)
          prime = p :: prime
    }
    prime.sortWith(_ < _)
  }

  def getEssentialPrimeImplicants(prime: Seq[Term], minterms: Seq[Term]): (Seq[Term], Seq[Term], Seq[Term]) = {
    val primeCovers = prime.map(p => minterms.filter(p.covers))
    for (((icover, pi), i) <- primeCovers.zip(prime).zipWithIndex) {
      for (((jcover, pj), j) <- primeCovers.zip(prime).zipWithIndex.drop(i + 1)) {
        if (icover.size > jcover.size && jcover.forall(pi.covers))
          return getEssentialPrimeImplicants(prime.filter(_ != pj), minterms)
      }
    }

    val essentiallyCovered = minterms.filter(t => prime.count(_.covers(t)) == 1)
    val essential = prime.filter(p => essentiallyCovered.exists(p.covers))
    val nonessential = prime.filterNot(essential.contains)
    val uncovered = minterms.filterNot(t => essential.exists(_.covers(t)))
    if (essential.isEmpty || uncovered.isEmpty)
      (essential, nonessential, uncovered)
    else {
      val (a, b, c) = getEssentialPrimeImplicants(nonessential, uncovered)
      (essential ++ a, b, c)
    }
  }

  def getCost(cover: Seq[Term], bits: Int): Int = cover.map(bits - _.mask.bitCount).sum

  def cheaper(a: List[Term], b: List[Term], bits: Int): Boolean = {
    val ca = getCost(a, bits)
    val cb = getCost(b, bits)

    @tailrec
    def listLess(a: List[Term], b: List[Term]): Boolean =
      b.nonEmpty && (a.isEmpty || a.head < b.head || a.head == b.head && listLess(a.tail, b.tail))

    ca < cb || ca == cb && listLess(a.sortWith(_ < _), b.sortWith(_ < _))
  }

  def getCover(implicants: Seq[Term], minterms: Seq[Term], bits: Int): Seq[Term] = {
    if (minterms.nonEmpty) {
      val cover = minterms.map(m => implicants.filter(_.covers(m)))
      val all = cover.tail.foldLeft(cover.head.map(Set(_)))((c0, c1) => c0.flatMap(a => c1.map(a + _)))
      all.map(_.toList).reduceLeft((a, b) => if (cheaper(a, b, bits)) a else b)
    } else
      Seq[Term]()
  }

  def stringify(s: Seq[Term], bits: Int): String =
    s.map(t =>
      (0 until bits)
        .map(i =>
          if ((t.mask & (1 << i)) != 0) "x"
          else ((t.value >> i) & 1).toString
        )
        .reduceLeft(_ + _)
        .reverse
    ).reduceLeft(_ + " + " + _)

  def apply(minterms: Seq[Term], dontcares: Seq[Term], bits: Int): Seq[Term] = {
    if (dontcares.isEmpty) {
      // As an elaboration performance optimization, don't be too clever if
      // there are no don't-cares; synthesis can figure it out.
      minterms
    } else {
      val prime = getPrimeImplicants(minterms ++ dontcares, bits)
      minterms.foreach(t => assert(prime.exists(_.covers(t))))
      val (eprime, prime2, uncovered) = getEssentialPrimeImplicants(prime, minterms)
      val cover = eprime ++ getCover(prime2, uncovered, bits)
      minterms.foreach(t => assert(cover.exists(_.covers(t)))) // sanity check
      cover
    }
  }
}
