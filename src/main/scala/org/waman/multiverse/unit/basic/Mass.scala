package org.waman.multiverse.unit.basic

import org.waman.multiverse._
import spire.math.{Fractional, Real}

class Mass[A: Fractional](val value: A, val unit: MassUnit)
    extends LinearQuantity[Mass[A], A, MassUnit] {

  override protected def newQuantity(value: A, unit: MassUnit): Mass[A] =
    new Mass(value, unit)
}

trait MassUnit extends LinearUnit[MassUnit]{
  import MassUnits.kg

  override def getSIUnit: MassUnit = kg
}

class SimpleMassUnit(val name: String, val interval: Real) extends MassUnit with SymbolByClassName[MassUnit]