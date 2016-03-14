package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.LengthUnit
import spire.math.Real

sealed trait TorqueUnit extends PhysicalUnit[TorqueUnit]{

  def unitInNewtonMetre: Real

  override def baseUnit = ForceUnit.Newton * LengthUnit.Metre
  override def valueInBaseUnit = unitInNewtonMetre
}

object TorqueUnit extends ConstantsDefined[TorqueUnit]{

  // intrinsic
  private[TorqueUnit]
  class IntrinsicTorqueUnit(name: String, val symbols: Seq[String], val unitInNewtonMetre: Real)
      extends TorqueUnit{

    def this(name: String, symbols: Seq[String], unit: TorqueUnit) =
      this(name, symbols, unit.unitInNewtonMetre)

    def this(name: String, symbols: Seq[String], factor: Real, unit: TorqueUnit) =
      this(name, symbols, factor * unit.unitInNewtonMetre)
  }


  override lazy val values = Seq()

  // ForceUnit * LengthUnit -> Torque
  private[TorqueUnit]
  class ForceDotLengthUnit(val firstUnit: ForceUnit, val secondUnit: LengthUnit)
      extends TorqueUnit with ProductUnit[TorqueUnit, ForceUnit, LengthUnit]{

    override lazy val unitInNewtonMetre: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: ForceUnit, unit2: LengthUnit): TorqueUnit =
    new ForceDotLengthUnit(unit1, unit2)
}