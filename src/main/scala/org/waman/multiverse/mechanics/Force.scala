package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPostfixOps, LengthUnit}
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

trait ForceDot[A]{
  import ForceUnit._

  protected def forceDot(forceUnit: ForceUnit): A

  def N  (dot: Dot): A = forceDot(Newton)
  def dyn(dot: Dot): A = forceDot(Dyne)
  def kgf(dot: Dot): A = forceDot(KiloGramForce)
  def kp (dot: Dot): A = forceDot(KiloGramForce)
}

class Force[A: Fractional](val value: A, val unit: ForceUnit)
  extends Quantity[A, ForceUnit]
    with ForcePostfixOps[A]
    with MultiplicativeByLength[Torque[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ForceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInNewton) / real(evalUnit.unitInNewton)

  override protected def forcePostfixOps(forceUnit: ForceUnit) = apply(forceUnit)

  override def *(lengthUnit: LengthUnit): Torque[A] = new Torque(value, unit * lengthUnit)
}

sealed abstract class ForceUnit(val symbol: String, val unitInNewton: Real)
  extends PhysicalUnit[ForceUnit]
  with MultiplicativeByLength[TorqueUnit]{

  override val baseUnit = ForceUnit.Newton
  override val inBaseUnitAccessor = () => unitInNewton

  override def *(lengthUnit: LengthUnit): TorqueUnit = TorqueUnit(this, lengthUnit)
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

trait ForceUnitInterpreter[A]
    extends ForcePostfixOps[Force[A]]
    with ForceDot[LengthPostfixOps[Torque[A]]]{

  def apply(unit: ForceUnit): Force[A]

  override protected def forcePostfixOps(forceUnit: ForceUnit) = apply(forceUnit)

  override protected def forceDot(forceUnit: ForceUnit) = new LengthPostfixOps[Torque[A]]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(forceUnit * lengthUnit)
  }

  def apply(unit: TorqueUnit): Torque[A]
}