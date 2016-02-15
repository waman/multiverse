package org.waman.multiverse

import spire.implicits._
import spire.math.{Fractional, Real}

trait ForcePostfixOps[A]{
  import ForceUnit._

  protected def forcePostfixOps(forceUnit: ForceUnit): A

  def N  : A = forcePostfixOps(Newton)
  def dyn: A = forcePostfixOps(Dyne)
  def kgf: A = forcePostfixOps(KiloGramForce)
  def kp : A = forcePostfixOps(KiloGramForce)
}

class Force[A: Fractional](val value: A, val unit: ForceUnit)
  extends Quantity[A, ForceUnit]
    with ForcePostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ForceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInNewton) / real(evalUnit.unitInNewton)

  override protected def forcePostfixOps(forceUnit: ForceUnit) = apply(forceUnit)
}

sealed abstract class ForceUnit(val symbol: String, val unitInNewton: Real)
  extends PhysicalUnit[ForceUnit]{

  override val baseUnit = ForceUnit.Newton
  override val inBaseUnitAccessor = () => unitInNewton
}

object ForceUnit{

  case object Newton        extends ForceUnit("N"  , 1)
  case object Dyne          extends ForceUnit("dyn", r"1e-5")
  case object KiloGramForce extends ForceUnit("kgf;kp", r"9.80665")
}

trait PredefinedForceUnit extends ForcePostfixOps[ForceUnit]{

  override protected def forcePostfixOps(forceUnit: ForceUnit) = forceUnit
}

object PredefinedForceUnit extends PredefinedForceUnit

trait ForceUnitInterpreter[A] extends ForcePostfixOps[Force[A]]{

  def apply(unit: ForceUnit): Force[A]

  override protected def forcePostfixOps(forceUnit: ForceUnit) = apply(forceUnit)
}