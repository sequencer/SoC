package org.chipsalliance.utils.crossing

/** Enumerates the types of clock crossings generally supported by Diplomatic bus protocols */
trait ClockCrossingType extends CrossingType {
  def sameClock: Boolean = this match {
    case _: SynchronousCrossing | _: CreditedCrossing => true
    case _ => false
  }
}
