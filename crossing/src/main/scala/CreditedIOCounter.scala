package org.chipsalliance.utils.crossing

import chisel3._
import chisel3.util.log2Ceil

class CreditedIOCounter(val init: Int, val depth: Int) {
  require(0 <= init)
  require(init <= depth)

  private val v = RegInit(init.U(log2Ceil(depth + 1).W))
  private val nextV = WireInit(v)

  val value = v + 0.U
  val nextValue = nextV + 0.U

  def full:  Bool = v === depth.U
  def empty: Bool = v === 0.U

  def update(credit: Bool, debit: Bool): Unit = {
    assert((!(credit && full) || debit) && (!(debit && empty) || credit))
    val next = Mux(credit, v + 1.U, v - 1.U)
    when(credit =/= debit) {
      nextV := next
      v := next
    }
  }

  def update(c: CreditedIO[_]): Unit = { update(c.credit, c.debit) }
}
