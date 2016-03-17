package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPer, AreaPostfixOps, AreaUnit}
import org.waman.multiverse.time.{DivisibleByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

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

trait KinematicViscosityFactory[A]
    extends KinematicViscosityPostfixOps[KinematicViscosity[A]]{

  def apply(unit: KinematicViscosityUnit): KinematicViscosity[A]

  override protected def kinematicViscosityPostfixOps(kinematicViscosityUnit: KinematicViscosityUnit) =
    apply(kinematicViscosityUnit)
}