package org.chipsalliance.utils.decoder.qmc

object SimplifyDC {
  def getImplicitDC(maxterms: Seq[Term], term: Term, bits: Int, above: Boolean): Term = {
    for (i <- 0 until bits) {
      var t: Term = null
      if (above && ((term.value | term.mask) & (BigInt(1) << i)) == 0)
        t = new Term(term.value | (BigInt(1) << i), term.mask)
      else if (!above && (term.value & (BigInt(1) << i)) != 0)
        t = new Term(term.value & ~(BigInt(1) << i), term.mask)
      if (t != null && !maxterms.exists(_.intersects(t)))
        return t
    }
    null
  }

  def getPrimeImplicants(minterms: Seq[Term], maxterms: Seq[Term], bits: Int): Seq[Term] = {
    var prime = List[Term]()
    minterms.foreach(_.prime = true)
    var mint = minterms.map(t => new Term(t.value, t.mask))
    val cols = (0 to bits).map(b => mint.filter(b == _.mask.bitCount))
    val table = cols.map(c => (0 to bits).map(b => collection.mutable.Set(c.filter(b == _.value.bitCount): _*)))

    for (i <- 0 to bits) {
      for (j <- 0 until bits - i) {
        table(i)(j).foreach(a =>
          table(i + 1)(j) ++= table(i)(j + 1)
            .filter(_.similar(a))
            .map(_.merge(a))
        )
      }
      for (j <- 0 until bits - i) {
        for (a <- table(i)(j).filter(_.prime)) {
          val dc = getImplicitDC(maxterms, a, bits, true)
          if (dc != null) table(i + 1)(j) += dc.merge(a)
        }
        for (a <- table(i)(j + 1).filter(_.prime)) {
          val dc = getImplicitDC(maxterms, a, bits, false)
          if (dc != null) table(i + 1)(j) += a.merge(dc)
        }
      }
      for (r <- table(i))
        for (p <- r; if p.prime)
          prime = p :: prime
    }
    prime.sortWith(_ < _)
  }

  def verify(cover: Seq[Term], minterms: Seq[Term], maxterms: Seq[Term]) = {
    assert(minterms.forall(t => cover.exists(_.covers(t))))
    assert(maxterms.forall(t => !cover.exists(_.intersects(t))))
  }

  def apply(minterms: Seq[Term], maxterms: Seq[Term], bits: Int) = {
    val prime = getPrimeImplicants(minterms, maxterms, bits)
    val (eprime, prime2, uncovered) = Simplify.getEssentialPrimeImplicants(prime, minterms)
    val cover = eprime ++ Simplify.getCover(prime2, uncovered, bits)
    verify(cover, minterms, maxterms)
    cover
  }
}
