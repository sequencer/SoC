// See LICENSE.SiFive for license details.

package org.chipsalliance.utils.regmapper

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Irrevocable, IrrevocableIO}
import org.chipsalliance.utils.crossing.{AsyncQueue, AsyncQueueParams, AsyncResetRegVec}

// A very simple flow control state machine, run in the specified clock domain
class BusyRegisterCrossing extends Module {
  class BusyRegisterCrossingBundle extends Bundle {
    val bypass:                 Bool = Input(Bool())
    val master_request_valid:   Bool = Input(Bool())
    val master_request_ready:   Bool = Output(Bool())
    val master_response_valid:  Bool = Output(Bool())
    val master_response_ready:  Bool = Input(Bool())
    val crossing_request_valid: Bool = Output(Bool())
    val crossing_request_ready: Bool = Input(Bool())
    // ... no crossing_response_ready; we are always ready
  }
  val io: BusyRegisterCrossingBundle = IO(new BusyRegisterCrossingBundle)

  val busy:   Bool = RegInit(false.B)
  val bypass: Bool = Reg(Bool())

  when(io.crossing_request_ready || Mux(busy, bypass, io.bypass)) {
    busy := Mux(busy, !io.master_response_ready, io.master_request_valid)
  }

  when(io.master_request_valid && io.master_request_ready) {
    bypass := io.bypass
  }

  io.crossing_request_valid := io.master_request_valid && !io.bypass && !busy
  io.master_request_ready := (io.bypass || io.crossing_request_ready) && !busy
  io.master_response_valid := (bypass || io.crossing_request_ready) && busy
}

class RegisterCrossingAssertion extends Module {
  class RegisterCrossingAssertionBundle extends Bundle {
    val master_bypass: Bool = Input(Bool())
    val slave_reset:   Bool = Input(Bool())
  }
  val io: RegisterCrossingAssertionBundle = IO(new RegisterCrossingAssertionBundle)

  val up: Bool = RegInit(false.B)
  up := !io.slave_reset

  assert(io.master_bypass || !up || !io.slave_reset)
}

// RegField should support connecting to one of these
class RegisterWriteIO[T <: Data](gen: T) extends Bundle {
  val request:  DecoupledIO[T] = Flipped(Decoupled(gen))
  val response: IrrevocableIO[Bool] = Irrevocable(Bool()) // ignore .bits

  override def cloneType: RegisterWriteIO.this.type = new RegisterWriteIO(gen).asInstanceOf[this.type]
}

// To turn off=>on a domain:
//  A. To turn disable the master domain
//    1. wait for all inflight traffic to resolve
//    2. assert master reset
//    3. (optional) stop the master clock
//    --- YOU MAY NOT TURN OFF POWER ---
//    4. re-enable the clock
//    5. deassert reset
//  B. To turn off the slave domain
//    1. assert bypass
//    2. wait for inflight traffic to resolve
//    3. assert slave reset
//    4. (optional) stop the slave clock
//    --- YOU MAY NOT TURN OFF POWER ---
//    5. re-enable the clock
//    6. deassert reset
//    7. deassert bypass
//
// If you need to cut power, use something that support isolation gates.

class RegisterWriteCrossingIO[T <: Data](gen: T) extends Bundle {
  // Master clock domain
  val master_clock: Clock = Input(Clock())
  val master_reset: Bool = Input(Bool())
  val master_port = new RegisterWriteIO(gen)
  // Bypass requests from the master to be noops
  val master_bypass: Bool = Input(Bool())
  // Slave clock domain
  val slave_clock:    Clock = Input(Clock())
  val slave_reset:    Bool = Input(Bool())
  val slave_register: T = Output(gen)
  val slave_valid:    Bool = Output(Bool()) // is high on 1st cycle slave_register has a new value
}

class RegisterWriteCrossing[T <: Data](gen: T, sync: Int = 3) extends Module {
  val io: RegisterWriteCrossingIO[T] = IO(new RegisterWriteCrossingIO(gen))
  // The crossing must only allow one item inflight at a time
  val control:  BusyRegisterCrossing = Module(new BusyRegisterCrossing)
  val crossing: AsyncQueue[T] = Module(new AsyncQueue(gen, AsyncQueueParams.singleton(sync)))

  control.clock := io.master_clock
  control.reset := io.master_reset
  crossing.io.enq_clock := io.master_clock
  crossing.io.enq_reset := io.master_reset
  crossing.io.deq_clock := io.slave_clock
  crossing.io.deq_reset := io.slave_reset

  control.io.bypass := io.master_bypass
  control.io.master_request_valid := io.master_port.request.valid
  control.io.master_response_ready := io.master_port.response.ready
  io.master_port.request.ready := control.io.master_request_ready
  io.master_port.response.valid := control.io.master_response_valid

  control.io.crossing_request_ready := crossing.io.enq.ready
  crossing.io.enq.valid := control.io.crossing_request_valid
  crossing.io.enq.bits := io.master_port.request.bits

  crossing.io.deq.ready := true.B
  io.slave_valid := crossing.io.deq.valid
  io.slave_register := crossing.io.deq.bits

  val assertion: RegisterCrossingAssertion = Module(new RegisterCrossingAssertion)
  assertion.clock := io.master_clock
  assertion.reset := io.master_reset
  assertion.io.master_bypass := io.master_bypass
  assertion.io.slave_reset := io.slave_reset
}

// RegField should support connecting to one of these
class RegisterReadIO[T <: Data](gen: T) extends Bundle {
  val request:  DecoupledIO[Bool] = Flipped(Decoupled(Bool())) // ignore .bits
  val response: IrrevocableIO[T] = Irrevocable(gen)

  override def cloneType: RegisterReadIO.this.type = new RegisterReadIO(gen).asInstanceOf[this.type]
}

class RegisterReadCrossingIO[T <: Data](gen: T) extends Bundle {
  // Master clock domain
  val master_clock: Clock = Input(Clock())
  val master_reset: Bool = Input(Bool())
  val master_port = new RegisterReadIO(gen)
  // Bypass requests from the master to be noops
  val master_bypass: Bool = Input(Bool())
  // Slave clock domain
  val slave_clock:    Clock = Input(Clock())
  val slave_reset:    Bool = Input(Bool())
  val slave_register: T = Input(gen)
}

class RegisterReadCrossing[T <: Data](gen: T, sync: Int = 3) extends Module {
  val io: RegisterReadCrossingIO[T] = IO(new RegisterReadCrossingIO(gen))
  // The crossing must only allow one item inflight at a time
  val control:  BusyRegisterCrossing = Module(new BusyRegisterCrossing)
  val crossing: AsyncQueue[T] = Module(new AsyncQueue(gen, AsyncQueueParams.singleton(sync)))

  control.clock := io.master_clock
  control.reset := io.master_reset
  crossing.io.deq_clock := io.master_clock
  crossing.io.deq_reset := io.master_reset
  crossing.io.enq_clock := io.slave_clock
  crossing.io.enq_reset := io.slave_reset

  control.io.bypass := io.master_bypass
  control.io.master_request_valid := io.master_port.request.valid
  control.io.master_response_ready := io.master_port.response.ready
  io.master_port.request.ready := control.io.master_request_ready
  io.master_port.response.valid := control.io.master_response_valid

  control.io.crossing_request_ready := crossing.io.deq.valid
  crossing.io.deq.ready := control.io.crossing_request_valid
  io.master_port.response.bits := crossing.io.deq.bits

  crossing.io.enq.valid := true.B
  crossing.io.enq.bits := io.slave_register

  val assertion: RegisterCrossingAssertion = Module(new RegisterCrossingAssertion)
  assertion.clock := io.master_clock
  assertion.reset := io.master_reset
  assertion.io.master_bypass := io.master_bypass
  assertion.io.slave_reset := io.slave_reset
}

/** Wrapper to create an
  *  asynchronously reset slave register which can be
  *  both read and written
  *  using crossing FIFOs.
  *  The reset and allow assertion & de-assertion
  *  should be synchronous to their respective
  *  domains.
  */

object AsyncRWSlaveRegField {

  def apply(
    master_clock:  Clock,
    master_reset:  Bool,
    slave_clock:   Clock,
    slave_reset:   Bool,
    width:         Int,
    init:          Int,
    name:          Option[String] = None,
    master_bypass: Bool = true.B,
    desc:          Option[RegFieldDesc] = None
  ): (UInt, RegField) = {

    val async_slave_reg = Module(new AsyncResetRegVec(width, init))
    name.foreach(async_slave_reg.suggestName(_))
    async_slave_reg.reset := slave_reset
    async_slave_reg.clock := slave_clock

    val wr_crossing = Module(new RegisterWriteCrossing(UInt(width.W)))
    name.foreach(n => wr_crossing.suggestName(s"${n}_wcrossing"))

    wr_crossing.io.master_clock := master_clock
    wr_crossing.io.master_reset := master_reset
    wr_crossing.io.master_bypass := master_bypass
    wr_crossing.io.slave_clock := slave_clock
    wr_crossing.io.slave_reset := slave_reset

    async_slave_reg.io.en := wr_crossing.io.slave_valid
    async_slave_reg.io.d := wr_crossing.io.slave_register

    val rd_crossing = Module(new RegisterReadCrossing(UInt(width.W)))
    name.foreach(n => rd_crossing.suggestName(s"${n}_rcrossing"))

    rd_crossing.io.master_clock := master_clock
    rd_crossing.io.master_reset := master_reset
    rd_crossing.io.master_bypass := master_bypass
    rd_crossing.io.slave_clock := slave_clock
    rd_crossing.io.slave_reset := slave_reset

    rd_crossing.io.slave_register := async_slave_reg.io.q

    (async_slave_reg.io.q, RegField(width, rd_crossing.io.master_port, wr_crossing.io.master_port, desc))
  }
}
