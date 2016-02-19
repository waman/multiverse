package org.waman.multiverse.fluid

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait PressurePostfixOps[A]{

  import PressureUnit._

  protected def pressurePostfixOps(pressureUnit: PressureUnit): A

  def Pa: A = pressurePostfixOps(Pascal)
}

class Pressure[A: Fractional](val value: A, val unit: PressureUnit)
  extends Quantity[A, PressureUnit]
    with PressurePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: PressureUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInPascal) / real(evalUnit.unitInPascal)


  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = apply(pressureUnit)
}

abstract class PressureUnit(val symbol: String, val unitInPascal: Real)
  extends PhysicalUnit[PressureUnit]{

  def this(symbol: String, factor: Real, pressureUnit: PressureUnit) =
    this(symbol, factor * pressureUnit.unitInPascal)

  override val baseUnit = PressureUnit.Pascal
  override val inBaseUnitAccessor = () => unitInPascal
}

object PressureUnit{

  case object Pascal extends PressureUnit("Pa", 1)
}

trait PredefinedPressureUnit extends PressurePostfixOps[PressureUnit]{

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = pressureUnit
}

object PredefinedPressureUnit extends PredefinedPressureUnit

trait PressureUnitInterpreter[A]
  extends PressurePostfixOps[Pressure[A]]{

  def apply(unit: PressureUnit): Pressure[A]

  override protected def pressurePostfixOps(pressureUnit: PressureUnit) = apply(pressureUnit)
}