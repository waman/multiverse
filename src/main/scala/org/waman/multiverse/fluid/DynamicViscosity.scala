package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait DynamicViscosityPostfixOps[A]{

  import DynamicViscosityUnit._

  protected def dynamicViscosityPostfixOps(dynamicViscosityUnit: DynamicViscosityUnit): A

  def P: A = dynamicViscosityPostfixOps(Poise)
}

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

sealed trait DynamicViscosityUnit extends PhysicalUnit[DynamicViscosityUnit]{

  def unitInPascalSecond: Real

  override lazy val baseUnit = PressureUnit.Pascal * TimeUnit.Second
  override lazy val inBaseUnitAccessor = () => unitInPascalSecond
}

object DynamicViscosityUnit{

  // Custom
  private[DynamicViscosityUnit]
  class DynamicViscosityUnitImpl(val symbol: String, val unitInPascalSecond: Real)
    extends DynamicViscosityUnit

  case object Poise extends DynamicViscosityUnitImpl("P", r"0.1")

  // Product (Pressure * Time)
  private[DynamicViscosityUnit]
  class ProductDynamicViscosityUnit(val firstUnit: PressureUnit, val secondUnit: TimeUnit)
    extends DynamicViscosityUnit with ProductUnit[DynamicViscosityUnit, PressureUnit, TimeUnit]{

    override lazy val unitInPascalSecond: Real =
      firstUnit.unitInPascal * secondUnit.unitInSecond
  }

  def apply(pUnit: PressureUnit, tUnit: TimeUnit): DynamicViscosityUnit =
    new ProductDynamicViscosityUnit(pUnit, tUnit)
}

trait PredefinedDynamicViscosityUnit extends DynamicViscosityPostfixOps[DynamicViscosityUnit]{

  override protected def dynamicViscosityPostfixOps(dynamicViscosityUnit: DynamicViscosityUnit) =
    dynamicViscosityUnit
}

object PredefinedDynamicViscosityUnit extends PredefinedDynamicViscosityUnit

trait DynamicViscosityUnitInterpreter[A]
  extends DynamicViscosityPostfixOps[DynamicViscosity[A]]{

  def apply(unit: DynamicViscosityUnit): DynamicViscosity[A]

  override protected def dynamicViscosityPostfixOps(dynamicViscosityUnit: DynamicViscosityUnit) =
    apply(dynamicViscosityUnit)
}