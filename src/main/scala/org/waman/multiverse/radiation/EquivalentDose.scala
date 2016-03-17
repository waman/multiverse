package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.time.{DivisibleByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class EquivalentDose[A: Fractional](val value: A, val unit: EquivalentDoseUnit)
  extends Quantity[A, EquivalentDoseUnit]
    with EquivalentDosePostfixOps[A]
    with DivisibleByTimeUnit[EquivalentDoseRate[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EquivalentDoseUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSievert) / real(evalUnit.unitInSievert)

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) =
    apply(equivalentDoseUnit)

  override def /(timeUnit: TimeUnit) = new EquivalentDoseRate(value, unit / timeUnit)
}

trait EquivalentDoseFactory[A]
    extends EquivalentDosePostfixOps[EquivalentDose[A]]
    with EquivalentDosePer[TimePostfixOps[EquivalentDoseRate[A]]]{

  def apply(unit: EquivalentDoseUnit): EquivalentDose[A]

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) =
    apply(equivalentDoseUnit)

  // EquivalentDose / Time -> EquivalentDoseRate
  def apply(unit: EquivalentDoseRateUnit) : EquivalentDoseRate[A]

  override protected def equivalentDosePer(equivalentDoseUnit: EquivalentDoseUnit) =
    new TimePostfixOps[EquivalentDoseRate[A]]{
      override protected def timePostfixOps(timeUnit: TimeUnit) = apply(equivalentDoseUnit / timeUnit)
    }
}