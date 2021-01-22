package org.chipsalliance.utils.crossing
import scala.language.implicitConversions

case class TriStateValue(value: Boolean, set: Boolean) {
  def update(orig: Boolean): Boolean = if (set) value else orig
}

object TriStateValue {
  implicit def apply(value: Boolean): TriStateValue = TriStateValue(value, set = true)
  def unset: TriStateValue = TriStateValue(value = false, set = false)
}
