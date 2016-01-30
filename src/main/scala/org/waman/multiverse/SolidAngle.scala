package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait SolidAnglePostfixOps[A]{
  def sr  : A
  def deg2: A
}

class SolidAngle[A: Fractional](val value: A, val unit: SolidAngleUnit)
  extends Quantity[A, SolidAngleUnit]
    with SolidAnglePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: SolidAngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.inSteradian) / real(evalUnit.inSteradian)

  override def sr   = apply(SolidAngleUnit.Steradian)
  override def deg2 = apply(SolidAngleUnit.SquareDegree)
}

abstract class SolidAngleUnit(val symbol: String, val inSteradian: Real)
  extends PhysicalUnit {

  override protected def baseUnit = SolidAngleUnit.Steradian
  override protected def inBaseUnitAccessor = () => inSteradian
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

  override def sr   = apply(SolidAngleUnit.Steradian)
  override def deg2 = apply(SolidAngleUnit.SquareDegree)
}