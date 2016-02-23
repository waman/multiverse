package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

class EquivalentDoseRate[A: Fractional](val value: A, val unit: EquivalentDoseRateUnit)
  extends Quantity[A, EquivalentDoseRateUnit]
    with EquivalentDosePostfixOps[DivisibleByTimeUnit[A]]
    with EquivalentDosePer[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EquivalentDoseRateUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSievertPerSecond) / real(evalUnit.unitInSievertPerSecond)

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) =
    new DivisibleByTimeUnit[A]{
      override def /(timeUnit: TimeUnit) = apply(equivalentDoseUnit / timeUnit)
    }

  override protected def equivalentDosePer(equivalentDoseUnit: EquivalentDoseUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(equivalentDoseUnit / timeUnit)
  }
}

sealed trait EquivalentDoseRateUnit extends PhysicalUnit[EquivalentDoseRateUnit]{

  def unitInSievertPerSecond: Real

  override def baseUnit = EquivalentDoseUnit.Sievert / TimeUnit.Second
  override def valueInBaseUnit = unitInSievertPerSecond
}

object EquivalentDoseRateUnit{

  // Quotient (EquivalentDose / Time)
  private[EquivalentDoseRateUnit]
  class QuotientEquivalentDoseRateUnit(val numeratorUnit: EquivalentDoseUnit, val denominatorUnit: TimeUnit)
    extends EquivalentDoseRateUnit with QuotientUnit[EquivalentDoseRateUnit, EquivalentDoseUnit, TimeUnit]{

    override lazy val unitInSievertPerSecond: Real =
      numeratorUnit.unitInSievert / denominatorUnit.unitInSecond
  }

  def apply(mUnit: EquivalentDoseUnit, vUnit: TimeUnit): EquivalentDoseRateUnit =
    new QuotientEquivalentDoseRateUnit(mUnit, vUnit)
}

trait PredefinedEquivalentDoseRateUnit

object PredefinedEquivalentDoseRateUnit extends PredefinedEquivalentDoseRateUnit

trait EquivalentDoseRateUnitInterpreter[A]
    extends UnitConverter[A]{

  def apply(unit: EquivalentDoseRateUnit): EquivalentDoseRate[A]
}
