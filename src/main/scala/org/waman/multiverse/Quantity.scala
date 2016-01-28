package org.waman.multiverse

trait Quantity[A, U <: PhysicalUnit]{
  val value: A
  val unit: U
  override def toString = s"$value (${unit.symbol})"
}