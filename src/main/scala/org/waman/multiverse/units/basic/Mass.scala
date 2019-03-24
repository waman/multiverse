package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.MassUnits
import spire.math.{Fractional, Real}

class Mass[A: Fractional](val value: A, val unit: MassUnit)
    extends ExtensiveQuantity[Mass[A], A, MassUnit] {

  override protected def newQuantity(value: A, unit: MassUnit): Mass[A] =
    new Mass(value, unit)
}

trait MassUnit extends ScaleUnit[MassUnit]{

  override def getSIUnit: MassUnit = MassUnits.kg
}

class SimpleMassUnit(val name: String, val intervalInSIUnit: Real) extends MassUnit