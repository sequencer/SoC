package tilelink

import chisel3.util.isPow2

case class TLChannelBeatBytes(a: Int, b: Int, c: Int, d: Int) {
  def members = Seq(a, b, c, d)

  require(members.forall { beatBytes =>
    isPow2(beatBytes)
  }, "Data channel width must be a power of 2")
}
