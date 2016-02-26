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
    with AreaPostfixOps[DivisibleByTimeUnit[A]]
    with AreaPer[TimePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: KinematicViscosityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSquareMetrePerSecond) / real(evalUnit.unitInSquareMetrePerSecond)

  override protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit) =
    apply(kinematicViscosityUnit)

  override protected def areaPostfixOps(areaUnit: AreaUnit) = new DivisibleByTimeUnit[A]{
    override def /(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }

  override protected def areaPer(areaUnit: AreaUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }
}

sealed trait KinematicViscosityUnit extends PhysicalUnit[KinematicViscosityUnit]{

  def unitInSquareMetrePerSecond: Real

  override def baseUnit = AreaUnit.SquareMetre / TimeUnit.Second
  override def valueInBaseUnit = unitInSquareMetrePerSecond
}

object KinematicViscosityUnit extends ConstantsDefined[KinematicViscosityUnit]{

  // Custom
  private[KinematicViscosityUnit]
  class IntrinsicKinematicViscosityUnit(val symbol: String, val unitInSquareMetrePerSecond: Real)
    extends KinematicViscosityUnit

  case object Stokes extends IntrinsicKinematicViscosityUnit("St", r"1e-4")

  override lazy val values = Seq(
    Stokes
  )

  // Quotient (Area / Time)
  private[KinematicViscosityUnit]
  class QuotientKinematicViscosityUnit(val numeratorUnit: AreaUnit, val denominatorUnit: TimeUnit)
    extends KinematicViscosityUnit with QuotientUnit[KinematicViscosityUnit, AreaUnit, TimeUnit]{

    override lazy val unitInSquareMetrePerSecond: Real =
      numeratorUnit.unitInSquareMetre / denominatorUnit.unitInSecond
  }

  def apply(aUnit: AreaUnit, tUnit: TimeUnit): KinematicViscosityUnit =
    new QuotientKinematicViscosityUnit(aUnit, tUnit)
}

trait PredefinedKinematicViscosityUnit extends KinematicViscosityPostfixOps[KinematicViscosityUnit]{

  override protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit) =
    kinematicViscosityUnit
}

object PredefinedKinematicViscosityUnit extends PredefinedKinematicViscosityUnit

trait KinematicViscosityFactory[A]
    extends KinematicViscosityPostfixOps[KinematicViscosity[A]]{

  def apply(unit: KinematicViscosityUnit): KinematicViscosity[A]

  override protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit) =
    apply(kinematicViscosityUnit)
}