package multiverse.unit.basic

import spire.math.Real
import multiverse.typeless.TypelessLinearUnit

class LengthPoweredAreaUnit(val baseUnit: LengthUnit, val aliases: Seq[String]) extends AreaUnit {
  val name: String = "square " + this.baseUnit.name
  val symbol: String = this.baseUnit.symbol + "²"
  val interval: Real = this.baseUnit.interval**2
  override def asTypeless: TypelessLinearUnit = this.baseUnit^2
}
