package org.waman.multiverse.radiation

import org.waman.multiverse._
import spire.implicits._
import spire.math.Fractional

class Radioactivity[A: Fractional](val value: A, val unit: RadioactivityUnit)
  extends Quantity[A, RadioactivityUnit]
    with RadioactivityPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: RadioactivityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInBecquerel) / real(evalUnit.unitInBecquerel)

  override protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit) = apply(radioactivityUnit)
}

trait RadioactivityFactory[A]
    extends RadioactivityPostfixOps[Radioactivity[A]]{

  def apply(unit: RadioactivityUnit): Radioactivity[A]

  override protected def radioactivityPostfixOps(radioactivityUnit: RadioactivityUnit) =
    apply(radioactivityUnit)
}