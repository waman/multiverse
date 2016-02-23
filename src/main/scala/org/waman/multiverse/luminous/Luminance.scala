package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait LuminancePostfixOps[A]{

  import LuminanceUnit._

  protected def luminancePostfixOps(luminanceUnit: LuminanceUnit): A

  def sb: A = luminancePostfixOps(Stilb)
}

class Luminance[A: Fractional](val value: A, val unit: LuminanceUnit)
    extends Quantity[A, LuminanceUnit]
    with LuminancePostfixOps[A]
    with LuminousIntensityPostfixOps[DivisibleByAreaUnit[A]]
    with LuminousIntensityPer[AreaPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCandelaPerSquareMetre) / real(evalUnit.unitInCandelaPerSquareMetre)

  override protected def luminancePostfixOps(luminanceUnit: LuminanceUnit) =
    apply(luminanceUnit)

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    new DivisibleByAreaUnit[A]{
      override def /(areaUnit: AreaUnit) = apply(luminousIntensityUnit / areaUnit)
    }

  override protected def luminousIntensityPer(luminousIntensityUnit: LuminousIntensityUnit) =
    new AreaPostfixOps[A]{
      override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousIntensityUnit / areaUnit)
    }
}

sealed trait LuminanceUnit extends PhysicalUnit[LuminanceUnit]{

  def unitInCandelaPerSquareMetre: Real

  override def baseUnit = LuminousIntensityUnit.Candela / AreaUnit.SquareMetre
  override def valueInBaseUnit = unitInCandelaPerSquareMetre
}

object LuminanceUnit{

  // Custom
  private[LuminanceUnit]
  class LuminanceUnitImpl(val symbol: String, val unitInCandelaPerSquareMetre: Real)
    extends LuminanceUnit

  case object Stilb extends LuminanceUnitImpl("sb", r"1e4")

  // Quotient (LuminousIntensity / SquareMetre)
  private[LuminanceUnit]
  class QuotientLuminanceUnit(val numeratorUnit: LuminousIntensityUnit, val denominatorUnit: AreaUnit)
    extends LuminanceUnit with QuotientUnit[LuminanceUnit, LuminousIntensityUnit, AreaUnit]{

    override lazy val unitInCandelaPerSquareMetre: Real =
      numeratorUnit.unitInCandela / denominatorUnit.unitInSquareMetre
  }

  def apply(liUnit: LuminousIntensityUnit, aUnit: AreaUnit): LuminanceUnit =
    new QuotientLuminanceUnit(liUnit, aUnit)
}

trait PredefinedLuminanceUnit extends LuminancePostfixOps[LuminanceUnit]{

  override protected def luminancePostfixOps(luminanceUnit: LuminanceUnit) =
    luminanceUnit
}

object PredefinedLuminanceUnit extends PredefinedLuminanceUnit

trait LuminanceUnitInterpreter[A]
    extends LuminancePostfixOps[Luminance[A]]{

  def apply(unit: LuminanceUnit): Luminance[A]

  override protected def luminancePostfixOps(luminanceUnit: LuminanceUnit) =
    apply(luminanceUnit)
}