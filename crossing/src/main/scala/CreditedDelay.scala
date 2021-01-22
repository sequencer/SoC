package org.chipsalliance.utils.crossing

/** Transmission delay in credit-debit systems.
  * debit delay is the number of cycles it takes debit+'bits' to transit the link.
  * credit delay is the number of cycles it takes credits to be returned.
  * round trip / total delay is the sum of debit and credit delay.
  * The system must have a positive total delay, otherwise you have a combinational loop.
  */
case class CreditedDelay(debit: Int, credit: Int) {
  val total: Int = debit + credit
  require(debit >= 0)
  require(credit >= 0)

  def flip: CreditedDelay = CreditedDelay(credit, debit)

  def +(that: CreditedDelay): CreditedDelay =
    CreditedDelay(debit + that.debit, credit + that.credit)

  override def toString = s"$debit:$credit"
}
