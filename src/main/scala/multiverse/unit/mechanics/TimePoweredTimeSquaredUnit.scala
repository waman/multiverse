package multiverse.unit.mechanics

import spire.math.Real
import multiverse.typeless.TypelessLinearUnit
import multiverse.unit.basic.TimeUnit

class TimePoweredTimeSquaredUnit(val baseUnit: TimeUnit, val aliases: Seq[String]) extends TimeSquaredUnit {
  val name: String = this.baseUnit.name + " squared"
  val symbol: String = this.baseUnit.symbol + "²"
  val interval: Real = this.baseUnit.interval**2
  override def asTypeless: TypelessLinearUnit = this.baseUnit^2
}