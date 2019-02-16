package org.waman.multiverse.units.mechanics

import org.waman.multiverse._
import org.waman.multiverse.predef.BasicUnits._
import org.waman.multiverse.units.basic.{LengthPerTimeVelocityUnit, TimeUnit, VelocityUnit}
import spire.math.{Fractional, Real}

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
    extends LinearQuantity[A, AccelerationUnit]

trait AccelerationUnit extends PhysicalUnit[AccelerationUnit] {
  override def getSIUnit: AccelerationUnit = m/s^2
}

class SimpleAccelerationUnit(val name: String, val unitValueInSIUnit: Real) extends AccelerationUnit

class VelocityPerTimeAccelerationUnit(val numeratorUnit: VelocityUnit, val denominatorUnit: TimeUnit)
    extends QuotientUnit[AccelerationUnit, VelocityUnit, TimeUnit] with AccelerationUnit{

  override protected def extractSymbol: String = {
    this.numeratorUnit match {
      case vu: LengthPerTimeVelocityUnit =>
        if(vu.denominatorUnit == this.denominatorUnit)
          s"${vu.numeratorUnit.symbol}/${vu.denominatorUnit.symbol}^2"
        else
          super.extractSymbol
      case _ => super.extractSymbol
    }
  }

  override val unitValueInSIUnit: Real = numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
}