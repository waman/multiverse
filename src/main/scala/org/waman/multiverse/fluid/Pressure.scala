package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.time.{MultiplicativeByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Pressure[A: Fractional](val value: A, val unit: PressureUnit)
  extends Quantity[A, PressureUnit]
    with PressurePostfixOps[A]
    with MultiplicativeByTimeUnit[DynamicViscosity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: PressureUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = apply(pressureUnit)

  override def *(timeUnit: TimeUnit) = new DynamicViscosity(value, unit * timeUnit)
}

trait PressureFactory[A]
    extends PressurePostfixOps[Pressure[A]]
    with PressureDot[TimePostfixOps[DynamicViscosity[A]]]{

  def apply(unit: PressureUnit): Pressure[A]

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = apply(pressureUnit)

  // Pressure * Time -> DynamicViscosity
  protected def apply(unit: DynamicViscosityUnit): DynamicViscosity[A]

  override protected def pressureDot(pressureUnit: PressureUnit) = new TimePostfixOps[DynamicViscosity[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(pressureUnit * timeUnit)
  }
}