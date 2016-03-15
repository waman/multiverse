package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.time.TimeUnit
import spire.math.Real

sealed trait EquivalentDoseRateUnit extends PhysicalUnit[EquivalentDoseRateUnit]{

  def unitInSievertPerSecond: Real

  override def baseUnit = EquivalentDoseUnit.Sievert / TimeUnit.Second
  override def valueInBaseUnit = unitInSievertPerSecond
}

object EquivalentDoseRateUnit extends ConstantsDefined[EquivalentDoseRateUnit]{

  // intrinsic
  private[EquivalentDoseRateUnit]
  class IntrinsicEquivalentDoseRateUnit(name: String, val symbols: Seq[String], val unitInSievertPerSecond: Real)
      extends EquivalentDoseRateUnit{

    def this(name: String, symbols: Seq[String], unit: EquivalentDoseRateUnit) =
      this(name, symbols, unit.unitInSievertPerSecond)

    def this(name: String, symbols: Seq[String], factor: Real, unit: EquivalentDoseRateUnit) =
      this(name, symbols, factor * unit.unitInSievertPerSecond)
  }



  override lazy val values = Seq()

  // EquivalentDoseUnit / TimeUnit -> EquivalentDoseRate
  private[EquivalentDoseRateUnit]
  class QuotientEquivalentDosePerTimeUnit(val numeratorUnit: EquivalentDoseUnit, val denominatorUnit: TimeUnit)
      extends EquivalentDoseRateUnit with QuotientUnit[EquivalentDoseRateUnit, EquivalentDoseUnit, TimeUnit]{

    override lazy val unitInSievertPerSecond: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: EquivalentDoseUnit, dUnit: TimeUnit): EquivalentDoseRateUnit =
    new QuotientEquivalentDosePerTimeUnit(nUnit, dUnit)
}