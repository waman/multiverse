package org.waman.multiverse.mass

import org.waman.multiverse._
import org.waman.multiverse.metric.{VolumePostfixOps, VolumeUnit}
import spire.implicits._
import spire.math.Fractional

class Mass[A: Fractional](val value: A, val unit: MassUnit)
  extends Quantity[A, MassUnit]
    with MassPostfixOps[A]
    with DivisibleByVolumeUnit[Density[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: MassUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInKiloGram) / real(evalUnit.unitInKiloGram)


  override protected def massPostfixOps(massUnit: MassUnit) = apply(massUnit)

  override def /(volumeUnit: VolumeUnit): Density[A] = new Density(value, unit / volumeUnit)
}

trait MassFactory[A]
  extends MassPostfixOps[Mass[A]]
    with MassPer[VolumePostfixOps[Density[A]]]{

  def apply(unit: MassUnit): Mass[A]

  override protected def massPostfixOps(massUnit: MassUnit) = apply(massUnit)

  // Mass / Volume -> Density
  def apply(unit: DensityUnit): Density[A]

  override protected def massPer(massUnit: MassUnit) = new VolumePostfixOps[Density[A]] {
    override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(massUnit / volumeUnit)
  }
}