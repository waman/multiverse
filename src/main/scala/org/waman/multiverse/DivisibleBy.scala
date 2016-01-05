package org.waman.multiverse

trait DivisibleBy[A, R]{
  def /(divUnit: A): R
}