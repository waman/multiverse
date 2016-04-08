package org.waman.multiverse.radiation

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.time._

sealed trait EquivalentDoseRateUnit extends PhysicalUnit[EquivalentDoseRateUnit]{

  override def getSIUnit = EquivalentDoseUnit.Sievert / TimeUnit.Second
}

object EquivalentDoseRateUnit extends ConstantsDefined[EquivalentDoseRateUnit]{

  // intrinsic
  private[EquivalentDoseRateUnit]
  class IntrinsicEquivalentDoseRateUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends EquivalentDoseRateUnit{

    def this(name: String, symbols: Seq[String], unit: EquivalentDoseRateUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: EquivalentDoseRateUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }



  override lazy val values = Seq()

  // EquivalentDoseUnit / TimeUnit -> EquivalentDoseRate
  private[EquivalentDoseRateUnit]
  class QuotientEquivalentDosePerTimeUnit(val numeratorUnit: EquivalentDoseUnit, val denominatorUnit: TimeUnit)
      extends EquivalentDoseRateUnit with QuotientUnit[EquivalentDoseRateUnit, EquivalentDoseUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: EquivalentDoseUnit, dUnit: TimeUnit): EquivalentDoseRateUnit =
    new QuotientEquivalentDosePerTimeUnit(nUnit, dUnit)
}

trait MultiplicativeByEquivalentDoseRateUnit[R]{
  def *(unit: EquivalentDoseRateUnit): R
}

trait DivisibleByEquivalentDoseRateUnit[R]{
  def /(unit: EquivalentDoseRateUnit): R
}