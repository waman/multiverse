package org.waman.multiverse.magnetic

import org.waman.multiverse._
import org.waman.multiverse.electric._
import org.waman.multiverse.metric.{DivisibleByAreaUnit, AreaPostfixOps, AreaUnit}
import org.waman.multiverse.time.{MultiplicativeByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Flux[A: Fractional](val value: A, val unit: FluxUnit)
  extends Quantity[A, FluxUnit]
    with FluxPostfixOps[A]
    with DivisibleByAreaUnit[FluxDensity[A]]
    with DivisibleByCurrentUnit[Inductance[A]]
    with VoltagePostfixOps[MultiplicativeByTimeUnit[A]]
    with VoltageDot[TimePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FluxUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInWeber) / real(evalUnit.unitInWeber)

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = apply(fluxUnit)

  override def /(areaUnit: AreaUnit) = new FluxDensity(value, unit / areaUnit)

  override def /(currentUnit: CurrentUnit) = new Inductance(value, unit / currentUnit)

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = new MultiplicativeByTimeUnit[A]{
    override def *(timeUnit: TimeUnit) = apply(voltageUnit * timeUnit)
  }

  override protected def voltageDot(voltageUnit: VoltageUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(voltageUnit * timeUnit)
  }
}

trait FluxFactory[A]
    extends FluxPostfixOps[Flux[A]]
    with FluxPer[AreaPostfixOps[FluxDensity[A]] with CurrentPostfixOps[Inductance[A]]]{

  def apply(unit: FluxUnit): Flux[A]

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = apply(fluxUnit)

  // Flux / Area -> FluxDensity
  // Flux / Current -> Inductance
  def apply(fdUnit: FluxDensityUnit): FluxDensity[A]
  def apply(iUnit: InductanceUnit): Inductance[A]

  override protected def fluxPer(fluxUnit: FluxUnit) =
    new AreaPostfixOps[FluxDensity[A]] with CurrentPostfixOps[Inductance[A]]{

      override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(fluxUnit / areaUnit)
      override protected def currentPostfixOps(currentUnit: CurrentUnit) = apply(fluxUnit / currentUnit)
    }
}