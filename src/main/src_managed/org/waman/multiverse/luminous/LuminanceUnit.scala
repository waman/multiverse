package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.AreaUnit
import spire.implicits._
import spire.math.Real

sealed trait LuminanceUnit extends PhysicalUnit[LuminanceUnit]{

  def unitInCandelaPerSquareMetre: Real

  override def baseUnit = LuminousIntensityUnit.Candela / AreaUnit.SquareMetre
  override def valueInBaseUnit = unitInCandelaPerSquareMetre
}

object LuminanceUnit extends ConstantsDefined[LuminanceUnit]{

  // intrinsic
  private[LuminanceUnit]
  class IntrinsicLuminanceUnit(name: String, val symbols: Seq[String], val unitInCandelaPerSquareMetre: Real)
      extends LuminanceUnit{

    def this(name: String, symbols: Seq[String], unit: LuminanceUnit) =
      this(name, symbols, unit.unitInCandelaPerSquareMetre)

    def this(name: String, symbols: Seq[String], factor: Real, unit: LuminanceUnit) =
      this(name, symbols, factor * unit.unitInCandelaPerSquareMetre)
  }


  case object Stilb extends IntrinsicLuminanceUnit("Stilb", Seq("sb"), r"1e4")
  case object Lambert extends IntrinsicLuminanceUnit("Lambert", Seq("Lb"), r"1e4" / Real.pi)
  case object ApoStilb extends IntrinsicLuminanceUnit("ApoStilb", Seq("asb"), r"1" / Real.pi)
  case object Skot extends IntrinsicLuminanceUnit("Skot", Seq("sk"), r"1e-3" / Real.pi)
  case object Bril extends IntrinsicLuminanceUnit("Bril", Seq("bril"), r"1e-7" / Real.pi)
  case object FootLambert extends IntrinsicLuminanceUnit("FootLambert", Seq("fLb"), r"1" / Real.pi, LuminousIntensityUnit.Candela / AreaUnit.SquareFoot)

  override lazy val values = Seq(Stilb, Lambert, ApoStilb, Skot, Bril, FootLambert)

  // LuminousIntensityUnit / AreaUnit -> Luminance
  private[LuminanceUnit]
  class QuotientLuminousIntensityPerAreaUnit(val numeratorUnit: LuminousIntensityUnit, val denominatorUnit: AreaUnit)
      extends LuminanceUnit with QuotientUnit[LuminanceUnit, LuminousIntensityUnit, AreaUnit]{

    override lazy val unitInCandelaPerSquareMetre: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: LuminousIntensityUnit, dUnit: AreaUnit): LuminanceUnit =
    new QuotientLuminousIntensityPerAreaUnit(nUnit, dUnit)
}

trait LuminancePostfixOps[A]{
  import LuminanceUnit._

  protected def luminancePostfixOps(unit: LuminanceUnit): A

  def sb : A = luminancePostfixOps(Stilb)
  def Lb : A = luminancePostfixOps(Lambert)
  def asb : A = luminancePostfixOps(ApoStilb)
  def sk : A = luminancePostfixOps(Skot)
  def bril : A = luminancePostfixOps(Bril)
  def fLb : A = luminancePostfixOps(FootLambert)
}

trait LuminanceDot[A]{
  import LuminanceUnit._

  protected def luminanceDot(unit: LuminanceUnit): A

  def sb(dot: Dot): A = luminanceDot(Stilb)
  def Lb(dot: Dot): A = luminanceDot(Lambert)
  def asb(dot: Dot): A = luminanceDot(ApoStilb)
  def sk(dot: Dot): A = luminanceDot(Skot)
  def bril(dot: Dot): A = luminanceDot(Bril)
  def fLb(dot: Dot): A = luminanceDot(FootLambert)
}

trait LuminancePer[A]{
  import LuminanceUnit._

  protected def luminancePer(unit: LuminanceUnit): A

  def sb(per: Per): A = luminancePer(Stilb)
  def Lb(per: Per): A = luminancePer(Lambert)
  def asb(per: Per): A = luminancePer(ApoStilb)
  def sk(per: Per): A = luminancePer(Skot)
  def bril(per: Per): A = luminancePer(Bril)
  def fLb(per: Per): A = luminancePer(FootLambert)
}

trait PredefinedLuminanceUnit extends LuminancePostfixOps[LuminanceUnit]{
  override protected def luminancePostfixOps(unit: LuminanceUnit) = unit
  
}

object PredefinedLuminanceUnit extends PredefinedLuminanceUnit
