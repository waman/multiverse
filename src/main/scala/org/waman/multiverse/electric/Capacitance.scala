package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.Fractional

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

trait CapacitanceFactory[A]
    extends CapacitancePostfixOps[Capacitance[A]]{

  def apply(unit: CapacitanceUnit): Capacitance[A]

  override protected def capacitancePostfixOps(capacitanceUnit: CapacitanceUnit) =
    apply(capacitanceUnit)
}