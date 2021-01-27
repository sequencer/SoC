package org.chipsalliance.utils.addressing

abstract class IdMap[T <: IdMapEntry] {
  protected val fmt: String
  val mapping:       Seq[T]
  def pretty: String = mapping.map(_.pretty(fmt)).mkString(",\n")
}
