package org.waman.multiverse.unit.basic

import spire.math.Real
import org.waman.multiverse.typeless.TypelessLinearUnit

class LengthPoweredVolumeUnit(val baseUnit: LengthUnit, val aliases: Seq[String]) extends VolumeUnit {
  val name: String = "cubic " + this.baseUnit.name
  val symbol: String = this.baseUnit.symbol + "³"
  val interval: Real = this.baseUnit.interval**3
  override def asTypeless: TypelessLinearUnit = this.baseUnit^3
}