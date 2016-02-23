package org.waman.multiverse.magnetic

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait FluxDensityPostfixOps[A]{

  import FluxDensityUnit._

  protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit): A

  def T: A = fluxDensityPostfixOps(Tesla)
}

class FluxDensity[A: Fractional](val value: A, val unit: FluxDensityUnit)
  extends Quantity[A, FluxDensityUnit]
    with FluxDensityPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FluxDensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInTesla) / real(evalUnit.unitInTesla)

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) = apply(fluxDensityUnit)
}

sealed abstract class FluxDensityUnit(val symbol: String, val unitInTesla: Real)
    extends PhysicalUnit[FluxDensityUnit]{

  override def baseUnit = FluxDensityUnit.Tesla
  override def valueInBaseUnit = unitInTesla
}

object FluxDensityUnit{

  case object Tesla extends FluxDensityUnit("T", 1)
}

trait PredefinedFluxDensityUnit extends FluxDensityPostfixOps[FluxDensityUnit]{

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) = fluxDensityUnit
}

object PredefinedFluxDensityUnit extends PredefinedFluxDensityUnit

trait FluxDensityUnitInterpreter[A]
    extends FluxDensityPostfixOps[FluxDensity[A]]{

  def apply(unit: FluxDensityUnit): FluxDensity[A]

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) =
    apply(fluxDensityUnit)
}