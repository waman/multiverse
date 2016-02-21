package org.waman.multiverse.magnetic

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait InductancePostfixOps[A]{

  import InductanceUnit._

  protected def inductancePostfixOps(inductanceUnit: InductanceUnit): A

  def H: A = inductancePostfixOps(Henry)
}

class Inductance[A: Fractional](val value: A, val unit: InductanceUnit)
  extends Quantity[A, InductanceUnit]
    with InductancePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: InductanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInHenry) / real(evalUnit.unitInHenry)

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) = apply(inductanceUnit)
}

sealed abstract class InductanceUnit(val symbol: String, val unitInHenry: Real)
    extends PhysicalUnit[InductanceUnit]{

  override val baseUnit = InductanceUnit.Henry
  override val inBaseUnitAccessor = () => unitInHenry
}

object InductanceUnit{

  case object Henry extends InductanceUnit("H", 1)
}

trait PredefinedInductanceUnit extends InductancePostfixOps[InductanceUnit]{

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) = inductanceUnit
}

object PredefinedInductanceUnit extends PredefinedInductanceUnit

trait InductanceUnitInterpreter[A]
    extends InductancePostfixOps[Inductance[A]]{

  def apply(unit: InductanceUnit): Inductance[A]

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) =
    apply(inductanceUnit)
}