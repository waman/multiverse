package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.mass.{DivisibleByMassUnit, MassPostfixOps, MassUnit}
import org.waman.multiverse.metric.{LengthPostfixOps, LengthUnit, MultiplicativeByLengthUnit}
import org.waman.multiverse.radiation.{Exposure, ExposureUnit}
import spire.implicits._
import spire.math.Fractional

class Charge[A: Fractional](val value: A, val unit: ChargeUnit)
  extends Quantity[A, ChargeUnit]
    with ChargePostfixOps[A]
    with MultiplicativeByLengthUnit[Dipole[A]]
    with DivisibleByMassUnit[Exposure[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ChargeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = apply(chargeUnit)

  override def *(lengthUnit: LengthUnit) = new Dipole(value, unit * lengthUnit)

  override def /(massUnit: MassUnit) = new Exposure(value, unit / massUnit)
}

trait ChargeFactory[A]
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