package org.chipsalliance.utils.crossing

case class CreditedCrossing(sourceDelay: CreditedDelay, sinkDelay: CreditedDelay) extends ClockCrossingType

object CreditedCrossing {
  def apply(delay: CreditedDelay): CreditedCrossing = CreditedCrossing(delay, delay.flip)
  def apply(): CreditedCrossing = CreditedCrossing(CreditedDelay(1, 1))
}
