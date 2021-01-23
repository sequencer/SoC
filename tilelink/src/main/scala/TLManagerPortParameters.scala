package tilelink

case class TLManagerPortParameters(
  managers:  Seq[TLManagerParameters],
  endSinkId: BigInt)
