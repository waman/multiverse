package org.waman.multiverse.metric

import org.waman.multiverse.PhysicalUnit
import spire.math._

trait LengthUnit extends PhysicalUnit[LengthUnit]

class SimpleLengthUnit(val unitValueInSIUnit: Real) extends LengthUnit

trait MultiplicativeByLengthUnit[R]{
  def *(unit: LengthUnit): R
}

trait DivisibleByLengthUnit[R]{
  def /(unit: LengthUnit): R
}