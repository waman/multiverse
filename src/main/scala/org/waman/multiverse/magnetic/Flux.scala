package org.waman.multiverse.magnetic

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait FluxPostfixOps[A]{

  import FluxUnit._

  protected def fluxPostfixOps(fluxUnit: FluxUnit): A

  def Wb: A = fluxPostfixOps(Weber)
}

class Flux[A: Fractional](val value: A, val unit: FluxUnit)
  extends Quantity[A, FluxUnit]
    with FluxPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FluxUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInWeber) / real(evalUnit.unitInWeber)

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = apply(fluxUnit)
}

sealed abstract class FluxUnit(val symbol: String, val unitInWeber: Real)
    extends PhysicalUnit[FluxUnit]{

  override def baseUnit = FluxUnit.Weber
  override def valueInBaseUnit = unitInWeber
}

object FluxUnit extends ConstantsDefined[FluxUnit]{

  case object Weber extends FluxUnit("Wb", 1)

  override lazy val values = Seq(
    Weber
  )
}

trait PredefinedFluxUnit extends FluxPostfixOps[FluxUnit]{

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = fluxUnit
}

object PredefinedFluxUnit extends PredefinedFluxUnit

trait FluxFactory[A]
    extends FluxPostfixOps[Flux[A]]{

  def apply(unit: FluxUnit): Flux[A]

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = apply(fluxUnit)
}