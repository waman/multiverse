package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait PressurePostfixOps[A]{

  import PressureUnit._

  protected def pressurePostfixOps(pressureUnit: PressureUnit): A

  def Pa: A = pressurePostfixOps(Pascal)
}

trait PressureDot[A]{

  import PressureUnit._

  protected def pressureDot(pressureUnit: PressureUnit): A

  def Pa(dot: Dot): A = pressureDot(Pascal)
}

class Pressure[A: Fractional](val value: A, val unit: PressureUnit)
  extends Quantity[A, PressureUnit]
    with PressurePostfixOps[A]
    with MultiplicativeByTimeUnit[DynamicViscosity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: PressureUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInPascal) / real(evalUnit.unitInPascal)

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = apply(pressureUnit)

  override def *(timeUnit: TimeUnit) = new DynamicViscosity(value, unit * timeUnit)
}

sealed abstract class PressureUnit(val symbol: String, val unitInPascal: Real)
    extends PhysicalUnit[PressureUnit]
    with MultiplicativeByTimeUnit[DynamicViscosityUnit]{

  def this(symbol: String, factor: Real, pressureUnit: PressureUnit) =
    this(symbol, factor * pressureUnit.unitInPascal)

  override val baseUnit = PressureUnit.Pascal
  override val inBaseUnitAccessor = () => unitInPascal

  override def *(timeUnit: TimeUnit) = DynamicViscosityUnit(this, timeUnit)
}

object PressureUnit{

  // Custom
  case object Pascal extends PressureUnit("Pa", 1)
}

trait PredefinedPressureUnit extends PressurePostfixOps[PressureUnit]{

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = pressureUnit
}

object PredefinedPressureUnit extends PredefinedPressureUnit

trait PressureUnitInterpreter[A]
    extends PressurePostfixOps[Pressure[A]]
    with PressureDot[TimePostfixOps[DynamicViscosity[A]]]{

  def apply(unit: PressureUnit): Pressure[A]

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = apply(pressureUnit)

  override protected def pressureDot(pressureUnit: PressureUnit) = new TimePostfixOps[DynamicViscosity[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(pressureUnit * timeUnit)
  }

  protected def apply(unit: DynamicViscosityUnit): DynamicViscosity[A]
}