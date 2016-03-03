package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait LuminancePostfixOps[A]{

  import LuminanceUnit._

  protected def luminancePostfixOps(luminanceUnit: LuminanceUnit): A

  def sb  : A = luminancePostfixOps(Stilb)
  def Lb  : A = luminancePostfixOps(Lambert)
  def asb : A = luminancePostfixOps(ApoStilb)
  def sk  : A = luminancePostfixOps(Skot)
  def bril: A = luminancePostfixOps(Bril)
  def fLb  : A = luminancePostfixOps(FootLambert)
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

object LuminanceUnit extends ConstantsDefined[LuminanceUnit]{

  // Custom
  private[LuminanceUnit]
  class IntrinsicLuminanceUnit(symbol: String, val unitInCandelaPerSquareMetre: Real)
      extends LuminanceUnit{

    def this(symbol: String, factor: Real, lUnit: LuminanceUnit) =
      this(symbol, factor * lUnit.unitInCandelaPerSquareMetre)

    override lazy val symbols = Seq(symbol)
  }

  case object Stilb extends IntrinsicLuminanceUnit("sb", r"1e4")

  case object Lambert  extends IntrinsicLuminanceUnit("Lb"  , r"1e4" / Real.pi)
  case object ApoStilb extends IntrinsicLuminanceUnit("asb" , r"1"   / Real.pi)
  case object Skot     extends IntrinsicLuminanceUnit("sk"  , r"1e-3" / Real.pi)
  case object Bril     extends IntrinsicLuminanceUnit("bril", r"1e-7" / Real.pi)

  case object FootLambert extends IntrinsicLuminanceUnit("fLb", r"1" / Real.pi,
    LuminousIntensityUnit.Candela / AreaUnit.SquareFoot)

  override lazy val values = Seq(
    Stilb,
    Lambert,
    ApoStilb,
    Skot,
    Bril,
    FootLambert
  )

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

trait LuminanceFactory[A]
    extends LuminancePostfixOps[Luminance[A]]{

  def apply(unit: LuminanceUnit): Luminance[A]

  override protected def luminancePostfixOps(luminanceUnit: LuminanceUnit) =
    apply(luminanceUnit)
}