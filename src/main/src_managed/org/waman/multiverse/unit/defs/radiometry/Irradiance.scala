package org.waman.multiverse.unit.defs.radiometry

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.mechanics._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.angle._

class Irradiance[A: Fractional](val value: A, val unit: IrradianceUnit)
    extends LinearQuantity[Irradiance[A], A, IrradianceUnit] {

  override protected def newQuantity(value: A, unit: IrradianceUnit): Irradiance[A] = new Irradiance(value, unit)

  def /(frequency: Frequency[A]): SpectralIrradiance[A] = new SpectralIrradiance(this.value / frequency.value, this.unit / frequency.unit)
}

/** None */
trait IrradianceUnit extends LinearUnit[IrradianceUnit]{

  override def getSIUnit: IrradianceUnit = IrradianceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = IrradianceUnit.dimension

  def /(frequencyUnit: FrequencyUnit): SpectralIrradianceUnit =
    new QuotientUnit[SpectralIrradianceUnit, IrradianceUnit, FrequencyUnit](IrradianceUnit.this, frequencyUnit) with SpectralIrradianceUnit
}

object IrradianceUnit extends UnitInfo[IrradianceUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1).withDefaultValue(0)

  val getSIUnit: IrradianceUnit = PowerUnit.getSIUnit / AreaUnit.getSIUnit

  def getUnits: Seq[IrradianceUnit] = Seq()
}


/** For no aliase or user defined units */
class SimpleIrradianceUnit(val name: String, val symbol: String, val interval: Real) extends IrradianceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultIrradianceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends IrradianceUnit
  