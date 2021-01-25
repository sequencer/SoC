package tilelink

case class TLChannelParameters(
  addressWidth: Int = 0,
  dataWidth:    Int = 0,
  sourceWidth:  Int = 0,
  sinkWidth:    Int = 0,
  sizeWidth:    Int = 0) {
  def maskWidth: Int = dataWidth / 8
}
