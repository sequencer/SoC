package tilelink

import scala.math.max

case class TLChannelParameters(
  addressWidth: Int = 0,
  dataWidth:    Int = 0,
  sourceWidth:  Int = 0,
  sinkWidth:    Int = 0,
  sizeWidth:    Int = 0) {
  def beatBytes: Int = dataWidth / 8
  def maskWidth: Int = dataWidth / 8
  def union(that: TLChannelParameters) = TLChannelParameters(
    addressWidth = max(addressWidth, that.addressWidth),
    dataWidth = max(dataWidth, that.dataWidth),
    sourceWidth = max(sourceWidth, that.sourceWidth),
    sinkWidth = max(sinkWidth, that.sinkWidth),
    sizeWidth = max(sizeWidth, that.sizeWidth)
  )
}
