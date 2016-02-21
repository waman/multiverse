package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPer, AreaPostfixOps, AreaUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait KinematicViscosityPostfixOps[A]{

  import KinematicViscosityUnit._

  protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit): A

  def St: A = kinematicViscosityPostfixOps(Stokes)
}

class KinematicViscosity[A: Fractional](val value: A, val unit: KinematicViscosityUnit)
    extends Quantity[A, KinematicViscosityUnit]
    with KinematicViscosityPostfixOps[A]
    with AreaPostfixOps[DivisibleByTime[A]]
    with AreaPer[TimePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: KinematicViscosityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSquareMetrePerSecond) / real(evalUnit.unitInSquareMetrePerSecond)

  override protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit) =
    apply(kinematicViscosityUnit)

  override protected def areaPostfixOps(areaUnit: AreaUnit) = new DivisibleByTime[A]{
    override def /(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }

  override protected def areaPer(areaUnit: AreaUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }
}

sealed trait KinematicViscosityUnit extends PhysicalUnit[KinematicViscosityUnit]{

  def unitInSquareMetrePerSecond: Real

  override lazy val baseUnit = AreaUnit.SquareMetre / TimeUnit.Second
  override lazy val inBaseUnitAccessor = () => unitInSquareMetrePerSecond
}

object KinematicViscosityUnit{

  // Custom
  private[KinematicViscosityUnit]
  class KinematicViscosityUnitImpl(val symbol: String, val unitInSquareMetrePerSecond: Real)
    extends KinematicViscosityUnit

  case object Stokes extends KinematicViscosityUnitImpl("St", r"1e-4")

  // Quotient (Area / Time)
  private[KinematicViscosityUnit]
  class ProductKinematicViscosityUnit(val firstUnit: AreaUnit, val secondUnit: TimeUnit)
    extends KinematicViscosityUnit with ProductUnit[KinematicViscosityUnit, AreaUnit, TimeUnit]{

    override lazy val unitInSquareMetrePerSecond: Real =
      firstUnit.unitInSquareMetre * secondUnit.unitInSecond
  }

  def apply(aUnit: AreaUnit, tUnit: TimeUnit): KinematicViscosityUnit =
    new ProductKinematicViscosityUnit(aUnit, tUnit)
}

trait PredefinedKinematicViscosityUnit extends KinematicViscosityPostfixOps[KinematicViscosityUnit]{

  override protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit) =
    kinematicViscosityUnit
}

object PredefinedKinematicViscosityUnit extends PredefinedKinematicViscosityUnit

trait KinematicViscosityUnitInterpreter[A]
    extends KinematicViscosityPostfixOps[KinematicViscosity[A]]
    with AreaPer[TimePostfixOps[KinematicViscosity[A]]]{

  def apply(unit: KinematicViscosityUnit): KinematicViscosity[A]

  override protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit) =
    apply(kinematicViscosityUnit)

  override protected def areaPer(areaUnit: AreaUnit) = new TimePostfixOps[KinematicViscosity[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }
}