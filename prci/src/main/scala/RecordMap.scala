// See LICENSE.SiFive for license details.
package org.chipsalliance.utils.prci

import chisel3._
import scala.collection.immutable.ListMap
import chisel3.internal.requireIsChiselType
import chisel3.experimental.DataMirror.internal.chiselTypeClone

final class RecordMap[T <: Data] private (eltMap: ListMap[String, T]) extends Record {

  eltMap.foreach { case (name, elt) => requireIsChiselType(elt, name) }

  // This is needed for Record
  val elements =
    ListMap[String, T]() ++ eltMap.mapValues(chiselTypeClone(_).asInstanceOf[T]) // mapValues return value is lazy

  def apply(x: Int) = elements.values.toSeq(x)
  def apply(x: String) = elements.get(x)
  def size = elements.size
  def data = elements.values

  override def cloneType: this.type = (new RecordMap(eltMap)).asInstanceOf[this.type]

}

object RecordMap {

  def apply[T <: Data](eltMap: ListMap[String, T]) = new RecordMap(eltMap)

  def apply[T <: Data](elements: (String, T)*): RecordMap[T] = {
    new RecordMap[T](ListMap[String, T](elements: _*))
  }
}
