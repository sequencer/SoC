package org.chipsalliance.utils.addressing

/** Options for describing the attributes of memory regions */
object RegionType {
  // Define the 'more relaxed than' ordering
  val cases = Seq(CACHED, TRACKED, UNCACHED, IDEMPOTENT, VOLATILE, PUT_EFFECTS, GET_EFFECTS)
  sealed trait T extends Ordered[T] {
    def compare(that: T): Int = cases.indexOf(that).compare(cases.indexOf(this))
  }

  case object CACHED extends T // an intermediate agent may have cached a copy of the region for you
  case object TRACKED extends T // the region may have been cached by another master, but coherence is being provided
  case object UNCACHED extends T // the region has not been cached yet, but should be cached when possible
  case object IDEMPOTENT extends T // gets return most recently put content, but content should not be cached
  case object VOLATILE extends T // content may change without a put, but puts and gets have no side effects
  case object PUT_EFFECTS extends T // puts produce side effects and so must not be combined/delayed
  case object GET_EFFECTS extends T // gets produce side effects and so must not be issued speculatively
}
