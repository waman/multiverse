package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.Fractional

class Current[A: Fractional](val value: A, val unit: CurrentUnit)
  extends Quantity[A, CurrentUnit]
    with CurrentPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: CurrentUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)


  override protected def currentPostfixOps(currentUnit: CurrentUnit) = apply(currentUnit)
}

trait CurrentFactory[A]
    extends CurrentPostfixOps[Current[A]]{

  def apply(unit: CurrentUnit): Current[A]

  override protected def currentPostfixOps(currentUnit: CurrentUnit) =
    apply(currentUnit)
}