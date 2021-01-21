package org.chipsalliance.utils.crossing

import chisel3.util.DecoupledIO
import chisel3._

/** A trait supplying a function allowing the contents of data
  * to be supressed for a time period, i.e. be blocked.
  */
trait Blockable[T <: Data] {
  def blockWhile(enable_blocking: Bool, data: T): T
}

object Blockable {
  implicit object BlockableBool extends Blockable[Bool] {
    def blockWhile(enable_blocking: Bool, x: Bool): Bool = x && !enable_blocking
  }

  implicit def BlockableDecoupled[T <: Data]: Blockable[DecoupledIO[T]] =
    new Blockable[DecoupledIO[T]] {
      def blockWhile(
        enable_blocking: Bool,
        data:            DecoupledIO[T]
      ): DecoupledIO[T] = {
        val res = Wire(chiselTypeOf(data))
        res.valid := data.valid
        data.ready := res.ready
        res.bits := data.bits
        when(enable_blocking) {
          res.valid := false.B
          data.ready := false.B
        }
        res
      }
    }

  implicit def BlockableVec[T <: Data: Blockable]: Blockable[Vec[T]] =
    new Blockable[Vec[T]] {
      def blockWhile(enable_blocking: Bool, data: Vec[T]): Vec[T] = {
        VecInit(
          data.map(x => implicitly[Blockable[T]].blockWhile(enable_blocking, x))
        )
      }
    }
}
