package tilelink

import chisel3.util.isPow2

case class TLChannelBeatBytes(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int]) {
  def members = Seq(a, b, c, d)

  members.collect {
    case Some(beatBytes) =>
      require(isPow2(beatBytes), "Data channel width must be a power of 2")
  }
}
