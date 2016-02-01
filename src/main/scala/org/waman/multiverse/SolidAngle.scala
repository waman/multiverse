package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait SolidAnglePostfixOps[A]{

  protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit): A

  def sr  : A = solidAnglePostfixOps(SolidAngleUnit.Steradian)
  def deg2: A = solidAnglePostfixOps(SolidAngleUnit.SquareDegree)
}

class SolidAngle[A: Fractional](val value: A, val unit: SolidAngleUnit)
  extends Quantity[A, SolidAngleUnit]
    with SolidAnglePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: SolidAngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSteradian) / real(evalUnit.unitInSteradian)

  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = apply(solidAngleUnit)
}

abstract class SolidAngleUnit(val symbol: String, val unitInSteradian: Real)
  extends PhysicalUnit {

  override protected val baseUnit = SolidAngleUnit.Steradian
  override protected val inBaseUnitAccessor = () => unitInSteradian
}

object SolidAngleUnit{
  case object Steradian    extends SolidAngleUnit("sr", r"1")
  case object SquareDegree extends SolidAngleUnit("deg2", (Real.pi / r"180")**2)
}

trait PredefinedSolidAngleUnit{
  val sr   = SolidAngleUnit.Steradian
  val deg2 = SolidAngleUnit.SquareDegree
}

object PredefinedSolidAngleUnit extends PredefinedSolidAngleUnit

trait SolidAngleUnitInterpreter[A]
  extends SolidAnglePostfixOps[SolidAngle[A]]{

  def apply(unit: SolidAngleUnit): SolidAngle[A]

  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = apply(solidAngleUnit)
}