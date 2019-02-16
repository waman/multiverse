package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.BasicUnits.kg
import spire.math.{Fractional, Real}

class Mass[A: Fractional](val value: A, val unit: MassUnit)
    extends ExtensiveQuantity[Mass[A], A, MassUnit] {

  override protected def newQuantity(value: A, unit: MassUnit): Mass[A] =
    new Mass(value, unit)
}

trait MassUnit extends PhysicalUnit[MassUnit]{

  override def getSIUnit: MassUnit = kg
}

class SimpleMassUnit(val name: String, val unitValueInSIUnit: Real) extends MassUnit