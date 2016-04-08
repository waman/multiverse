package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.electric.{ChargePer, ChargePostfixOps, ChargeUnit}
import org.waman.multiverse.mass.{DivisibleByMassUnit, MassPostfixOps, MassUnit}
import spire.implicits._
import spire.math.Fractional

class Exposure[A: Fractional](val value: A, val unit: ExposureUnit)
  extends Quantity[A, ExposureUnit]
    with ExposurePostfixOps[A]
    with ChargePostfixOps[DivisibleByMassUnit[A]]
    with ChargePer[MassPostfixOps[A]]
    with UnitConverter[A] {

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ExposureUnit): A =
    if (unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def exposurePostfixOps(exposureUnit: ExposureUnit) = apply(exposureUnit)

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = new DivisibleByMassUnit[A] {
    override def /(massUnit: MassUnit) = apply(chargeUnit / massUnit)
  }

  override protected def chargePer(chargeUnit: ChargeUnit) = new MassPostfixOps[A] {
    override protected def massPostfixOps(massUnit: MassUnit) = apply(chargeUnit / massUnit)
  }
}

trait ExposureFactory[A]
    extends ExposurePostfixOps[Exposure[A]]{

  def apply(unit: ExposureUnit): Exposure[A]

  override protected def exposurePostfixOps(exposureUnit: ExposureUnit) =
    apply(exposureUnit)
}