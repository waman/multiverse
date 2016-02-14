package org.waman.multiverse

import org.waman.multiverse.MultiverseUtil.twoPi
import spire.implicits._
import spire.math.{Fractional, Real}

trait SolidAnglePostfixOps[A]{
  import SolidAngleUnit._

  protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit): A

  def sr  : A = solidAnglePostfixOps(Steradian)
  def deg2: A = solidAnglePostfixOps(SquareDegree)
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
  case object SquareDegree extends SolidAngleUnit("deg2", (twoPi / r"360")**2)
}

trait PredefinedSolidAngleUnit extends SolidAnglePostfixOps[SolidAngleUnit]{
  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = solidAngleUnit
}

object PredefinedSolidAngleUnit extends PredefinedSolidAngleUnit

trait SolidAngleUnitInterpreter[A]
  extends SolidAnglePostfixOps[SolidAngle[A]]{

  def apply(unit: SolidAngleUnit): SolidAngle[A]

  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = apply(solidAngleUnit)
}