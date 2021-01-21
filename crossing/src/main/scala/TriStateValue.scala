package org.chipsalliance.utils.crossing

case class TriStateValue(value: Boolean, set: Boolean) {
  def update(orig: Boolean) = if (set) value else orig
}

object TriStateValue {
  implicit def apply(value: Boolean): TriStateValue = TriStateValue(value, true)
  def unset = TriStateValue(false, false)
}
