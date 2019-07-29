package org.waman.multiverse.units.mechanics

import org.waman.multiverse._
import org.waman.multiverse.predef.mechanics.EnergyUnitObjects
import spire.math.Fractional

class Energy[A: Fractional](val value: A, val unit: EnergyUnit)
    extends ExtensiveQuantity[Energy[A], A, EnergyUnit] {

  override protected def newQuantity(value: A, unit: EnergyUnit): Energy[A] =
    new Energy(value, unit)
}

trait EnergyUnit extends ScaleUnit[EnergyUnit]{

  override def getSIUnit: EnergyUnit = EnergyUnitObjects.joule
}