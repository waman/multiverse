package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.Fractional

class Resistance[A: Fractional](val value: A, val unit: ResistanceUnit)
  extends Quantity[A, ResistanceUnit]
    with ResistancePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ResistanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInOhm) / real(evalUnit.unitInOhm)

  override protected def resistancePostfixOps(resistanceUnit: ResistanceUnit) = apply(resistanceUnit)
}

trait ResistanceFactory[A]
    extends ResistancePostfixOps[Resistance[A]]{

  def apply(unit: ResistanceUnit): Resistance[A]

  override protected def resistancePostfixOps(resistanceUnit: ResistanceUnit) =
    apply(resistanceUnit)
}