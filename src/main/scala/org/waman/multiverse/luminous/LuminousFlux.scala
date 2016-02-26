package org.waman.multiverse.luminous

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait LuminousFluxPostfixOps[A]{

  import LuminousFluxUnit._

  protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit): A

  def lm: A = luminousFluxPostfixOps(Lumen)
}

class LuminousFlux[A: Fractional](val value: A, val unit: LuminousFluxUnit)
  extends Quantity[A, LuminousFluxUnit]
    with LuminousFluxPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminousFluxUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInLumen) / real(evalUnit.unitInLumen)

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) = apply(luminousFluxUnit)
}

sealed abstract class LuminousFluxUnit(val symbol: String, val unitInLumen: Real)
    extends PhysicalUnit[LuminousFluxUnit]{

  override def baseUnit = LuminousFluxUnit.Lumen
  override def valueInBaseUnit = unitInLumen
}

object LuminousFluxUnit extends ConstantsDefined[LuminousFluxUnit]{

  case object Lumen extends LuminousFluxUnit("lm", 1)

  override lazy val values = Seq(
    Lumen
  )
}

trait PredefinedLuminousFluxUnit extends LuminousFluxPostfixOps[LuminousFluxUnit]{

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) = luminousFluxUnit
}

object PredefinedLuminousFluxUnit extends PredefinedLuminousFluxUnit

trait LuminousFluxFactory[A]
    extends LuminousFluxPostfixOps[LuminousFlux[A]]{

  def apply(unit: LuminousFluxUnit): LuminousFlux[A]

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) =
    apply(luminousFluxUnit)
}