package org.chipsalliance.utils.addressing

trait IdMapEntry {
  def name:                    String
  def from:                    IdRange
  def to:                      IdRange
  def isCache:                 Boolean
  def requestFifo:             Boolean
  def maxTransactionsInFlight: Option[Int]
  def pretty(fmt: String) =
    if (from ne to) { // if the subclass uses the same reference for both from and to, assume its format string has an arity of 5
      fmt.format(
        to.start,
        to.end,
        from.start,
        from.end,
        s""""$name"""",
        if (isCache) " [CACHE]" else "",
        if (requestFifo) " [FIFO]" else ""
      )
    } else {
      fmt.format(
        from.start,
        from.end,
        s""""$name"""",
        if (isCache) " [CACHE]" else "",
        if (requestFifo) " [FIFO]" else ""
      )
    }
}
