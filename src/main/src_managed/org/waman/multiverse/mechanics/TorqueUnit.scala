package org.waman.multiverse.mechanics

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._

sealed trait TorqueUnit extends PhysicalUnit[TorqueUnit]{

  override def getSIUnit = ForceUnit.Newton * LengthUnit.Metre
}

object TorqueUnit extends ConstantsDefined[TorqueUnit]{

  // intrinsic
  private[TorqueUnit]
  class IntrinsicTorqueUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends TorqueUnit{

    def this(name: String, symbols: Seq[String], unit: TorqueUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: TorqueUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }



  override lazy val values = Seq()

  // ForceUnit * LengthUnit -> Torque
  private[TorqueUnit]
  class ProductForceDotLengthUnit(val firstUnit: ForceUnit, val secondUnit: LengthUnit)
      extends TorqueUnit with ProductUnit[TorqueUnit, ForceUnit, LengthUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: ForceUnit, unit2: LengthUnit): TorqueUnit =
    new ProductForceDotLengthUnit(unit1, unit2)
}

trait MultiplicativeByTorqueUnit[R]{
  def *(unit: TorqueUnit): R
}

trait DivisibleByTorqueUnit[R]{
  def /(unit: TorqueUnit): R
}