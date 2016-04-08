package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.metric.{MultiplicativeByLengthUnit, LengthPostfixOps, LengthUnit}
import spire.implicits._
import spire.math.Fractional

class Dipole[A: Fractional](val value: A, val unit: DipoleUnit)
  extends Quantity[A, DipoleUnit]
    with DipolePostfixOps[A]
    with ChargePostfixOps[MultiplicativeByLengthUnit[A]]
    with ChargeDot[LengthPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: DipoleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def dipolePostfixOps(dipoleUnit: DipoleUnit) = apply(dipoleUnit)

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = new MultiplicativeByLengthUnit[A]{
    override def *(lengthUnit: LengthUnit) = apply(chargeUnit * lengthUnit)
  }

  override protected def chargeDot(chargeUnit: ChargeUnit) = new LengthPostfixOps[A]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(chargeUnit * lengthUnit)
  }
}

trait DipoleFactory[A]
    extends DipolePostfixOps[Dipole[A]]{

  def apply(unit: DipoleUnit): Dipole[A]

  override protected def dipolePostfixOps(dipoleUnit: DipoleUnit) = apply(dipoleUnit)
}