package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait LuminousIntensityPostfixOps[A]{

  import LuminousIntensityUnit._

  protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit): A

  def cd: A = luminousIntensityPostfixOps(Candela)
}

trait LuminousIntensityPer[A]{

  import LuminousIntensityUnit._

  protected def luminousIntensityPer(luminousIntensityUnit: LuminousIntensityUnit): A

  def cd(per: Per): A = luminousIntensityPer(Candela)
}

class LuminousIntensity[A: Fractional](val value: A, val unit: LuminousIntensityUnit)
  extends Quantity[A, LuminousIntensityUnit]
    with LuminousIntensityPostfixOps[A]
    with DivisibleByAreaUnit[Luminance[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminousIntensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCandela) / real(evalUnit.unitInCandela)

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    apply(luminousIntensityUnit)

  override def /(areaUnit: AreaUnit) = new Luminance[A](value, unit / areaUnit)
}

sealed abstract class LuminousIntensityUnit(val symbol: String, val unitInCandela: Real)
    extends PhysicalUnit[LuminousIntensityUnit]
    with DivisibleByAreaUnit[LuminanceUnit]{

  override def baseUnit = LuminousIntensityUnit.Candela
  override def valueInBaseUnit = unitInCandela

  override def /(areaUnit: AreaUnit) = LuminanceUnit(this, areaUnit)
}

object LuminousIntensityUnit extends ConstantsDefined[LuminousIntensityUnit]{

  case object Candela extends LuminousIntensityUnit("cd", 1)

  override lazy val values = Seq(
    Candela
  )
}

trait PredefinedLuminousIntensityUnit extends LuminousIntensityPostfixOps[LuminousIntensityUnit]{

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    luminousIntensityUnit
}

object PredefinedLuminousIntensityUnit extends PredefinedLuminousIntensityUnit

trait LuminousIntensityFactory[A]
    extends LuminousIntensityPostfixOps[LuminousIntensity[A]]
    with LuminousIntensityPer[AreaPostfixOps[Luminance[A]]]{

  def apply(unit: LuminousIntensityUnit): LuminousIntensity[A]

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    apply(luminousIntensityUnit)

  // LuminousIntensity / Area -> Luminance
  def apply(unit: LuminanceUnit): Luminance[A]

  override protected def luminousIntensityPer(luminousIntensityUnit: LuminousIntensityUnit) =
    new AreaPostfixOps[Luminance[A]]{
      override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousIntensityUnit / areaUnit)
    }
}