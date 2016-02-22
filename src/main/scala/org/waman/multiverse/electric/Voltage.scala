package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait VoltagePostfixOps[A]{

  import VoltageUnit._

  protected def voltagePostfixOps(voltageUnit: VoltageUnit): A

  def V: A = voltagePostfixOps(Voltage)
}

trait VoltageDot[A]{

  import VoltageUnit._

  protected def voltageDot(voltageUnit: VoltageUnit): A

  def C(dot: Dot): A = voltageDot(Voltage)
}

class Voltage[A: Fractional](val value: A, val unit: VoltageUnit)
  extends Quantity[A, VoltageUnit]
    with VoltagePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VoltageUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCoulomb) / real(evalUnit.unitInCoulomb)

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = apply(voltageUnit)
}

sealed abstract class VoltageUnit(val symbol: String, val unitInCoulomb: Real)
    extends PhysicalUnit[VoltageUnit]{

  override val baseUnit = VoltageUnit.Voltage
  override val inBaseUnitAccessor = () => unitInCoulomb
}

object VoltageUnit{

  case object Voltage extends VoltageUnit("V", 1)
}

trait PredefinedVoltageUnit extends VoltagePostfixOps[VoltageUnit]{

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = voltageUnit
}

object PredefinedVoltageUnit extends PredefinedVoltageUnit

trait VoltageUnitInterpreter[A]
    extends VoltagePostfixOps[Voltage[A]]{

  def apply(unit: VoltageUnit): Voltage[A]

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = apply(voltageUnit)
}