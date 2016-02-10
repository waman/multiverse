package org.waman.multiverse

import spire.implicits._
import spire.math.{Fractional, Real}

trait TimeSquaredPostfixOps[A]{

  protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit): A
  
  def s2: A = timeSquaredPostfixOps(TimeSquaredUnit.SecondSquared)
}

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
  extends Quantity[A, TimeSquaredUnit]
    with TimeSquaredPostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TimeSquaredUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSecondSquared) / real(evalUnit.unitInSecondSquared)

  override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit) = apply(timeSquaredUnit)
}

sealed abstract class TimeSquaredUnit(val symbol: String, val unitInSecondSquared: Real)
  extends PhysicalUnit {

  def this(symbol: String, factor: Real, timeSquaredUnit: TimeSquaredUnit) =
    this(symbol, factor * timeSquaredUnit.unitInSecondSquared)

  override protected val baseUnit = TimeSquaredUnit.SecondSquared
  override protected val inBaseUnitAccessor = () => unitInSecondSquared
}

object TimeSquaredUnit{

  case object SecondSquared extends TimeSquaredUnit("s2" , r"1")
}

trait PredefinedTimeSquaredUnit extends TimeSquaredPostfixOps[TimeSquaredUnit]{
  override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit) = timeSquaredUnit
}

object PredefinedTimeSquaredUnit extends PredefinedTimeSquaredUnit

trait TimeSquaredUnitInterpreter[A] extends TimeSquaredPostfixOps[TimeSquared[A]]{

  def apply(unit: TimeSquaredUnit): TimeSquared[A]

  override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit) = apply(timeSquaredUnit)
}