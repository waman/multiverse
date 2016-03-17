package org.waman.multiverse.magnetic

import org.waman.multiverse._
import org.waman.multiverse.electric.{DivisibleByCurrentUnit, CurrentPostfixOps, CurrentUnit}
import spire.implicits._
import spire.math.Fractional

class Inductance[A: Fractional](val value: A, val unit: InductanceUnit)
  extends Quantity[A, InductanceUnit]
    with InductancePostfixOps[A]
    with FluxPostfixOps[DivisibleByCurrentUnit[A]]
    with FluxPer[CurrentPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: InductanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInHenry) / real(evalUnit.unitInHenry)

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) = apply(inductanceUnit)

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = new DivisibleByCurrentUnit[A]{
    override def /(currentUnit: CurrentUnit) = apply(fluxUnit / currentUnit)
  }

  override protected def fluxPer(fluxUnit: FluxUnit) = new CurrentPostfixOps[A]{
    override protected def currentPostfixOps(currentUnit: CurrentUnit) = apply(fluxUnit / currentUnit)
  }
}

trait InductanceFactory[A]
    extends InductancePostfixOps[Inductance[A]]{

  def apply(unit: InductanceUnit): Inductance[A]

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) =
    apply(inductanceUnit)
}