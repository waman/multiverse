package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.fluid.KinematicViscosity
import org.waman.multiverse.unit.fluid.KinematicViscosityUnit

class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends LinearQuantity[Area[A], A, AreaUnit] {

  override protected def newQuantity(value: A, unit: AreaUnit): Area[A] = new Area(value, unit)
  def *(length: Length[A]): Volume[A] = new Volume(this.value * length.value, this.unit * length.unit)

  def /(time: Time[A]): KinematicViscosity[A] = new KinematicViscosity(this.value / time.value, this.unit / time.unit)

}

trait AreaUnit extends LinearUnit[AreaUnit]{

  override def getSIUnit: AreaUnit = AreaUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AreaUnit.dimension

  def *(lengthUnit: LengthUnit): VolumeUnit =
    new ProductUnit[VolumeUnit, AreaUnit, LengthUnit](AreaUnit.this, lengthUnit) with VolumeUnit

  def /(timeUnit: TimeUnit): KinematicViscosityUnit =
    new QuotientUnit[KinematicViscosityUnit, AreaUnit, TimeUnit](AreaUnit.this, timeUnit) with KinematicViscosityUnit

}

object AreaUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 2).withDefaultValue(0)

  val getSIUnit: AreaUnit = LengthUnit.getSIUnit * LengthUnit.getSIUnit

  import AreaUnitObjects._
  def getUnits: Seq[AreaUnit] =
    Seq(square_metre, square_yoctometre, square_zeptometre, square_attometre, square_femtometre, square_picometre, square_nanometre, square_micrometre, square_millimetre, square_centimetre, square_decimetre, square_decametre, square_hectometre, square_kilometre, square_megametre, square_gigametre, square_terametre, square_petametre, square_exametre, square_zettametre, square_yottametre, square_foot, square_inch, are, hectare)
}



class DefaultAreaUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaUnit

object AreaUnitObjects{

  val square_metre: AreaUnit = LengthUnitObjects.metre.squared
  val square_yoctometre: AreaUnit = LengthUnitObjects.yoctometre.squared
  val square_zeptometre: AreaUnit = LengthUnitObjects.zeptometre.squared
  val square_attometre: AreaUnit = LengthUnitObjects.attometre.squared
  val square_femtometre: AreaUnit = LengthUnitObjects.femtometre.squared
  val square_picometre: AreaUnit = LengthUnitObjects.picometre.squared
  val square_nanometre: AreaUnit = LengthUnitObjects.nanometre.squared
  val square_micrometre: AreaUnit = LengthUnitObjects.micrometre.squared
  val square_millimetre: AreaUnit = LengthUnitObjects.millimetre.squared
  val square_centimetre: AreaUnit = LengthUnitObjects.centimetre.squared
  val square_decimetre: AreaUnit = LengthUnitObjects.decimetre.squared
  val square_decametre: AreaUnit = LengthUnitObjects.decametre.squared
  val square_hectometre: AreaUnit = LengthUnitObjects.hectometre.squared
  val square_kilometre: AreaUnit = LengthUnitObjects.kilometre.squared
  val square_megametre: AreaUnit = LengthUnitObjects.megametre.squared
  val square_gigametre: AreaUnit = LengthUnitObjects.gigametre.squared
  val square_terametre: AreaUnit = LengthUnitObjects.terametre.squared
  val square_petametre: AreaUnit = LengthUnitObjects.petametre.squared
  val square_exametre: AreaUnit = LengthUnitObjects.exametre.squared
  val square_zettametre: AreaUnit = LengthUnitObjects.zettametre.squared
  val square_yottametre: AreaUnit = LengthUnitObjects.yottametre.squared
  val square_foot: AreaUnit = LengthUnitObjects.foot.squared
  val square_inch: AreaUnit = LengthUnitObjects.inch.squared
  final object are extends DefaultAreaUnit("are", "a", Nil, r"1e2")
  final object hectare extends DefaultAreaUnit("hectare", "ha", Nil, r"1e4")
}

object AreaUnits{
  def `m²`: AreaUnit = AreaUnitObjects.square_metre
  def m2: AreaUnit = AreaUnitObjects.square_metre
  def `ym²`: AreaUnit = AreaUnitObjects.square_yoctometre
  def ym2: AreaUnit = AreaUnitObjects.square_yoctometre
  def `zm²`: AreaUnit = AreaUnitObjects.square_zeptometre
  def zm2: AreaUnit = AreaUnitObjects.square_zeptometre
  def `am²`: AreaUnit = AreaUnitObjects.square_attometre
  def am2: AreaUnit = AreaUnitObjects.square_attometre
  def `fm²`: AreaUnit = AreaUnitObjects.square_femtometre
  def fm2: AreaUnit = AreaUnitObjects.square_femtometre
  def `pm²`: AreaUnit = AreaUnitObjects.square_picometre
  def pm2: AreaUnit = AreaUnitObjects.square_picometre
  def `nm²`: AreaUnit = AreaUnitObjects.square_nanometre
  def nm2: AreaUnit = AreaUnitObjects.square_nanometre
  def `μm²`: AreaUnit = AreaUnitObjects.square_micrometre
  def μm2: AreaUnit = AreaUnitObjects.square_micrometre
  def `mcm²`: AreaUnit = AreaUnitObjects.square_micrometre
  def mcm2: AreaUnit = AreaUnitObjects.square_micrometre
  def `mm²`: AreaUnit = AreaUnitObjects.square_millimetre
  def mm2: AreaUnit = AreaUnitObjects.square_millimetre
  def `cm²`: AreaUnit = AreaUnitObjects.square_centimetre
  def cm2: AreaUnit = AreaUnitObjects.square_centimetre
  def `dm²`: AreaUnit = AreaUnitObjects.square_decimetre
  def dm2: AreaUnit = AreaUnitObjects.square_decimetre
  def `dam²`: AreaUnit = AreaUnitObjects.square_decametre
  def dam2: AreaUnit = AreaUnitObjects.square_decametre
  def `hm²`: AreaUnit = AreaUnitObjects.square_hectometre
  def hm2: AreaUnit = AreaUnitObjects.square_hectometre
  def `km²`: AreaUnit = AreaUnitObjects.square_kilometre
  def km2: AreaUnit = AreaUnitObjects.square_kilometre
  def `Km²`: AreaUnit = AreaUnitObjects.square_kilometre
  def Km2: AreaUnit = AreaUnitObjects.square_kilometre
  def `Mm²`: AreaUnit = AreaUnitObjects.square_megametre
  def Mm2: AreaUnit = AreaUnitObjects.square_megametre
  def `Gm²`: AreaUnit = AreaUnitObjects.square_gigametre
  def Gm2: AreaUnit = AreaUnitObjects.square_gigametre
  def `Tm²`: AreaUnit = AreaUnitObjects.square_terametre
  def Tm2: AreaUnit = AreaUnitObjects.square_terametre
  def `Pm²`: AreaUnit = AreaUnitObjects.square_petametre
  def Pm2: AreaUnit = AreaUnitObjects.square_petametre
  def `Em²`: AreaUnit = AreaUnitObjects.square_exametre
  def Em2: AreaUnit = AreaUnitObjects.square_exametre
  def `Zm²`: AreaUnit = AreaUnitObjects.square_zettametre
  def Zm2: AreaUnit = AreaUnitObjects.square_zettametre
  def `Ym²`: AreaUnit = AreaUnitObjects.square_yottametre
  def Ym2: AreaUnit = AreaUnitObjects.square_yottametre
  def `ft²`: AreaUnit = AreaUnitObjects.square_foot
  def ft2: AreaUnit = AreaUnitObjects.square_foot
  def `in²`: AreaUnit = AreaUnitObjects.square_inch
  def in2: AreaUnit = AreaUnitObjects.square_inch
  def a: AreaUnit = AreaUnitObjects.are
  def ha: AreaUnit = AreaUnitObjects.hectare
}