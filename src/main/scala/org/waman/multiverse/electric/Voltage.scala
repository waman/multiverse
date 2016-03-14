package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.magnetic.{Flux, FluxUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Voltage[A: Fractional](val value: A, val unit: VoltageUnit)
  extends Quantity[A, VoltageUnit]
    with VoltagePostfixOps[A]
    with MultiplicativeByTimeUnit[Flux[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VoltageUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInVolt) / real(evalUnit.unitInVolt)

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = apply(voltageUnit)

  override def *(timeUnit: TimeUnit) = new Flux(value, unit * timeUnit)
}

trait VoltageFactory[A]
    extends VoltagePostfixOps[Voltage[A]]
    with VoltageDot[TimePostfixOps[Flux[A]]]{

  def apply(unit: VoltageUnit): Voltage[A]

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = apply(voltageUnit)

  // Volt * Time -> Flux
  def apply(unit: FluxUnit): Flux[A]

  override protected def voltageDot(voltageUnit: VoltageUnit) = new TimePostfixOps[Flux[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(voltageUnit * timeUnit)
  }
}