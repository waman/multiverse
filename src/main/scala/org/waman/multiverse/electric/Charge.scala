package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.metric.LengthUnit
import spire.implicits._
import spire.math.{Fractional, Real}

trait ChargePostfixOps[A]{

  import ChargeUnit._

  protected def chargePostfixOps(chargeUnit: ChargeUnit): A

  def C: A = chargePostfixOps(Coulomb)
}

trait ChargeDot[A]{

  import ChargeUnit._

  protected def chargeDot(chargeUnit: ChargeUnit): A

  def C(dot: Dot): A = chargeDot(Coulomb)
}

class Charge[A: Fractional](val value: A, val unit: ChargeUnit)
  extends Quantity[A, ChargeUnit]
    with ChargePostfixOps[A]
    with MultiplicativeByLength[Dipole[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ChargeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCoulomb) / real(evalUnit.unitInCoulomb)

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = apply(chargeUnit)

  override def *(lengthUnit: LengthUnit) = new Dipole(value, unit * lengthUnit)
}

sealed abstract class ChargeUnit(val symbol: String, val unitInCoulomb: Real)
    extends PhysicalUnit[ChargeUnit]
    with MultiplicativeByLength[DipoleUnit]{

  override val baseUnit = ChargeUnit.Coulomb
  override val inBaseUnitAccessor = () => unitInCoulomb

  override def *(lengthUnit: LengthUnit) = DipoleUnit(this, lengthUnit)
}

object ChargeUnit{

  case object Coulomb extends ChargeUnit("C", 1)
}

trait PredefinedChargeUnit extends ChargePostfixOps[ChargeUnit]{

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = chargeUnit
}

object PredefinedChargeUnit extends PredefinedChargeUnit

trait ChargeUnitInterpreter[A]
    extends ChargePostfixOps[Charge[A]]{

  def apply(unit: ChargeUnit): Charge[A]

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) =
    apply(chargeUnit)
}