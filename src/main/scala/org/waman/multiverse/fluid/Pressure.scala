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

sealed abstract class PressureUnit(val symbols: Seq[String], val unitInPascal: Real)
    extends PhysicalUnit[PressureUnit]
    with MultiplicativeByTimeUnit[DynamicViscosityUnit]{

  def this(symbol: String, unitInPascal: Real) = this(Seq(symbol), unitInPascal)

  def this(symbols: Seq[String], factor: Real, pressureUnit: PressureUnit) =
    this(symbols, factor * pressureUnit.unitInPascal)

  def this(symbol: String, factor: Real, pressureUnit: PressureUnit) =
    this(Seq(symbol), factor, pressureUnit)

  override def baseUnit = PressureUnit.Pascal
  override def valueInBaseUnit = unitInPascal

  override def *(timeUnit: TimeUnit) = DynamicViscosityUnit(this, timeUnit)
}

object PressureUnit extends ConstantsDefined[PressureUnit]{

  // intrinsic
  case object Pascal extends PressureUnit("Pa", 1)

  override lazy val values = Seq(
    Pascal
  )
}

trait PredefinedPressureUnit extends PressurePostfixOps[PressureUnit]{

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = pressureUnit
}

object PredefinedPressureUnit extends PredefinedPressureUnit

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