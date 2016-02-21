package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait CurrentPostfixOps[A]{

  import CurrentUnit._

  protected def currentPostfixOps(currentUnit: CurrentUnit): A

  def A: A = currentPostfixOps(Ampere)
}

class Current[A: Fractional](val value: A, val unit: CurrentUnit)
  extends Quantity[A, CurrentUnit]
    with CurrentPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: CurrentUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInAmpere) / real(evalUnit.unitInAmpere)


  override protected def currentPostfixOps(currentUnit: CurrentUnit) = apply(currentUnit)
}

sealed abstract class CurrentUnit(val symbol: String, val unitInAmpere: Real)
  extends PhysicalUnit[CurrentUnit]{

  def this(symbol: String, factor: Real, currentUnit: CurrentUnit) =
    this(symbol, factor * currentUnit.unitInAmpere)

  override val baseUnit = CurrentUnit.Ampere
  override val inBaseUnitAccessor = () => unitInAmpere
}

object CurrentUnit{

  case object Ampere extends CurrentUnit("A", 1)
}

trait PredefinedCurrentUnit extends CurrentPostfixOps[CurrentUnit]{

  override protected def currentPostfixOps(currentUnit: CurrentUnit) = currentUnit
}

object PredefinedCurrentUnit extends PredefinedCurrentUnit

trait CurrentUnitInterpreter[A]
    extends CurrentPostfixOps[Current[A]]{

  def apply(unit: CurrentUnit): Current[A]

  override protected def currentPostfixOps(currentUnit: CurrentUnit) =
    apply(currentUnit)
}