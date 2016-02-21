package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait CapacitancePostfixOps[A]{

  import CapacitanceUnit._

  protected def capacitancePostfixOps(capacitanceUnit: CapacitanceUnit): A

  def F: A = capacitancePostfixOps(Farad)
}

class Capacitance[A: Fractional](val value: A, val unit: CapacitanceUnit)
  extends Quantity[A, CapacitanceUnit]
    with CapacitancePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: CapacitanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInFarad) / real(evalUnit.unitInFarad)

  override protected def capacitancePostfixOps(capacitanceUnit: CapacitanceUnit) = apply(capacitanceUnit)
}

sealed abstract class CapacitanceUnit(val symbol: String, val unitInFarad: Real)
    extends PhysicalUnit[CapacitanceUnit]{

  override val baseUnit = CapacitanceUnit.Farad
  override val inBaseUnitAccessor = () => unitInFarad
}

object CapacitanceUnit{

  case object Farad extends CapacitanceUnit("F", 1)
}

trait PredefinedCapacitanceUnit extends CapacitancePostfixOps[CapacitanceUnit]{

  override protected def capacitancePostfixOps(capacitanceUnit: CapacitanceUnit) = capacitanceUnit
}

object PredefinedCapacitanceUnit extends PredefinedCapacitanceUnit

trait CapacitanceUnitInterpreter[A]
    extends CapacitancePostfixOps[Capacitance[A]]{

  def apply(unit: CapacitanceUnit): Capacitance[A]

  override protected def capacitancePostfixOps(capacitanceUnit: CapacitanceUnit) =
    apply(capacitanceUnit)
}