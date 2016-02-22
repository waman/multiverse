package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.mass.{MassPostfixOps, MassUnit}
import org.waman.multiverse.metric.{LengthPostfixOps, LengthUnit}
import org.waman.multiverse.radiation.{Exposure, ExposureUnit}
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

trait ChargePer[A]{

  import ChargeUnit._

  protected def chargePer(chargeUnit: ChargeUnit): A

  def C(per: Per): A = chargePer(Coulomb)
}

class Charge[A: Fractional](val value: A, val unit: ChargeUnit)
  extends Quantity[A, ChargeUnit]
    with ChargePostfixOps[A]
    with MultiplicativeByLengthUnit[Dipole[A]]
    with DivisibleByMassUnit[Exposure[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ChargeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCoulomb) / real(evalUnit.unitInCoulomb)

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = apply(chargeUnit)

  override def *(lengthUnit: LengthUnit) = new Dipole(value, unit * lengthUnit)

  override def /(massUnit: MassUnit) = new Exposure(value, unit / massUnit)
}

sealed abstract class ChargeUnit(val symbol: String, val unitInCoulomb: Real)
    extends PhysicalUnit[ChargeUnit]
    with MultiplicativeByLengthUnit[DipoleUnit]
    with DivisibleByMassUnit[ExposureUnit]{

  override val baseUnit = ChargeUnit.Coulomb
  override val inBaseUnitAccessor = () => unitInCoulomb

  override def *(lengthUnit: LengthUnit) = DipoleUnit(this, lengthUnit)

  override def /(massUnit: MassUnit) = ExposureUnit(this, massUnit)
}

object ChargeUnit{

  case object Coulomb extends ChargeUnit("C", 1)
}

trait PredefinedChargeUnit extends ChargePostfixOps[ChargeUnit]{

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = chargeUnit
}

object PredefinedChargeUnit extends PredefinedChargeUnit

trait ChargeUnitInterpreter[A]
    extends ChargePostfixOps[Charge[A]]
    with ChargeDot[LengthPostfixOps[Dipole[A]]]
    with ChargePer[MassPostfixOps[Exposure[A]]]{

  def apply(unit: ChargeUnit): Charge[A]

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) =
    apply(chargeUnit)

  // Charge * Length -> Dipole
  protected def apply(unit: DipoleUnit): Dipole[A]

  override protected def chargeDot(chargeUnit: ChargeUnit) = new LengthPostfixOps[Dipole[A]]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(chargeUnit * lengthUnit)
  }

  // Charge / Mass -> Exposure
  protected def apply(unit: ExposureUnit): Exposure[A]

  override protected def chargePer(chargeUnit: ChargeUnit) = new MassPostfixOps[Exposure[A]]{
    override protected def massPostfixOps(massUnit: MassUnit) = apply(chargeUnit / massUnit)
  }
}