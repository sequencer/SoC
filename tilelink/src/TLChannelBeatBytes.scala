package tilelink

import chisel3.util.isPow2

case class TLChannelBeatBytes(a: Int, b: Int, c: Int, d: Int) {
  def members: Seq[Int] = Seq(a, b, c, d).filter((_: Int) != 0)

  require(
    members.forall { beatBytes: Int =>
      isPow2(beatBytes)
    },
    "Data channel width must be a power of 2"
  )
}
