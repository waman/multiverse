package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.time.{MultiplicativeByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class DynamicViscosity[A: Fractional](val value: A, val unit: DynamicViscosityUnit)
    extends Quantity[A, DynamicViscosityUnit]
    with DynamicViscosityPostfixOps[A]
    with PressurePostfixOps[MultiplicativeByTimeUnit[A]]
    with PressureDot[TimePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: DynamicViscosityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInPascalSecond) / real(evalUnit.unitInPascalSecond)

  override protected def dynamicViscosityPostfixOps(dynamicViscosityUnit: DynamicViscosityUnit) =
    apply(dynamicViscosityUnit)

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = new MultiplicativeByTimeUnit[A]{
    override def *(timeUnit: TimeUnit) = apply(pressureUnit * timeUnit)
  }

  override protected def pressureDot(pressureUnit: PressureUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(pressureUnit * timeUnit)
  }
}

trait DynamicViscosityFactory[A]
  extends DynamicViscosityPostfixOps[DynamicViscosity[A]]{

  def apply(unit: DynamicViscosityUnit): DynamicViscosity[A]

  override protected def dynamicViscosityPostfixOps(dynamicViscosityUnit: DynamicViscosityUnit) =
    apply(dynamicViscosityUnit)
}