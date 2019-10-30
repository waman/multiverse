package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._
class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends LinearQuantity[Area[A], A, AreaUnit] {

  override protected def newQuantity(value: A, unit: AreaUnit): Area[A] = new Area(value, unit)
             def *(length: Length[A]): Volume[A] = new Volume(this.value * length.value, this.unit * length.unit)

}

trait AreaUnit extends LinearUnit[AreaUnit]{
  override def getSIUnit: AreaUnit = AreaUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AreaUnit.dimension

  def *(lengthUnit: LengthUnit): VolumeUnit =
    new ProductUnit[VolumeUnit, AreaUnit, LengthUnit](AreaUnit.this, lengthUnit) with VolumeUnit

}

object AreaUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 2).withDefaultValue(0)

  val getSIUnit: AreaUnit = LengthUnit.getSIUnit * LengthUnit.getSIUnit

import AreaUnitObjects._
  def getUnits: Seq[AreaUnit] =
    Seq(metre_squared, yoctometre_squared, zeptometre_squared, attometre_squared, femtometre_squared, picometre_squared, nanometre_squared, micrometre_squared, millimetre_squared, centimetre_squared, decimetre_squared, decametre_squared, hectometre_squared, kilometre_squared, megametre_squared, gigametre_squared, terametre_squared, petametre_squared, exametre_squared, zettametre_squared, yottametre_squared, foot_squared, are, hectare)
}



class DefaultAreaUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaUnit

object AreaUnitObjects{

  val metre_squared: AreaUnit = LengthUnitObjects.metre.squared
  val yoctometre_squared: AreaUnit = LengthUnitObjects.yoctometre.squared
  val zeptometre_squared: AreaUnit = LengthUnitObjects.zeptometre.squared
  val attometre_squared: AreaUnit = LengthUnitObjects.attometre.squared
  val femtometre_squared: AreaUnit = LengthUnitObjects.femtometre.squared
  val picometre_squared: AreaUnit = LengthUnitObjects.picometre.squared
  val nanometre_squared: AreaUnit = LengthUnitObjects.nanometre.squared
  val micrometre_squared: AreaUnit = LengthUnitObjects.micrometre.squared
  val millimetre_squared: AreaUnit = LengthUnitObjects.millimetre.squared
  val centimetre_squared: AreaUnit = LengthUnitObjects.centimetre.squared
  val decimetre_squared: AreaUnit = LengthUnitObjects.decimetre.squared
  val decametre_squared: AreaUnit = LengthUnitObjects.decametre.squared
  val hectometre_squared: AreaUnit = LengthUnitObjects.hectometre.squared
  val kilometre_squared: AreaUnit = LengthUnitObjects.kilometre.squared
  val megametre_squared: AreaUnit = LengthUnitObjects.megametre.squared
  val gigametre_squared: AreaUnit = LengthUnitObjects.gigametre.squared
  val terametre_squared: AreaUnit = LengthUnitObjects.terametre.squared
  val petametre_squared: AreaUnit = LengthUnitObjects.petametre.squared
  val exametre_squared: AreaUnit = LengthUnitObjects.exametre.squared
  val zettametre_squared: AreaUnit = LengthUnitObjects.zettametre.squared
  val yottametre_squared: AreaUnit = LengthUnitObjects.yottametre.squared
  val foot_squared: AreaUnit = LengthUnitObjects.foot.squared
  final object are extends DefaultAreaUnit("are", "a", Nil, r"1e2")
  final object hectare extends DefaultAreaUnit("hectare", "ha", Nil, r"1e4")
}

object AreaUnits{
  def `m²`: AreaUnit = AreaUnitObjects.metre_squared
  def m2: AreaUnit = AreaUnitObjects.metre_squared
  def `ym²`: AreaUnit = AreaUnitObjects.yoctometre_squared
  def ym2: AreaUnit = AreaUnitObjects.yoctometre_squared
  def `zm²`: AreaUnit = AreaUnitObjects.zeptometre_squared
  def zm2: AreaUnit = AreaUnitObjects.zeptometre_squared
  def `am²`: AreaUnit = AreaUnitObjects.attometre_squared
  def am2: AreaUnit = AreaUnitObjects.attometre_squared
  def `fm²`: AreaUnit = AreaUnitObjects.femtometre_squared
  def fm2: AreaUnit = AreaUnitObjects.femtometre_squared
  def `pm²`: AreaUnit = AreaUnitObjects.picometre_squared
  def pm2: AreaUnit = AreaUnitObjects.picometre_squared
  def `nm²`: AreaUnit = AreaUnitObjects.nanometre_squared
  def nm2: AreaUnit = AreaUnitObjects.nanometre_squared
  def `μm²`: AreaUnit = AreaUnitObjects.micrometre_squared
  def μm2: AreaUnit = AreaUnitObjects.micrometre_squared
  def `mcm²`: AreaUnit = AreaUnitObjects.micrometre_squared
  def mcm2: AreaUnit = AreaUnitObjects.micrometre_squared
  def `mm²`: AreaUnit = AreaUnitObjects.millimetre_squared
  def mm2: AreaUnit = AreaUnitObjects.millimetre_squared
  def `cm²`: AreaUnit = AreaUnitObjects.centimetre_squared
  def cm2: AreaUnit = AreaUnitObjects.centimetre_squared
  def `dm²`: AreaUnit = AreaUnitObjects.decimetre_squared
  def dm2: AreaUnit = AreaUnitObjects.decimetre_squared
  def `dam²`: AreaUnit = AreaUnitObjects.decametre_squared
  def dam2: AreaUnit = AreaUnitObjects.decametre_squared
  def `hm²`: AreaUnit = AreaUnitObjects.hectometre_squared
  def hm2: AreaUnit = AreaUnitObjects.hectometre_squared
  def `km²`: AreaUnit = AreaUnitObjects.kilometre_squared
  def km2: AreaUnit = AreaUnitObjects.kilometre_squared
  def `Km²`: AreaUnit = AreaUnitObjects.kilometre_squared
  def Km2: AreaUnit = AreaUnitObjects.kilometre_squared
  def `Mm²`: AreaUnit = AreaUnitObjects.megametre_squared
  def Mm2: AreaUnit = AreaUnitObjects.megametre_squared
  def `Gm²`: AreaUnit = AreaUnitObjects.gigametre_squared
  def Gm2: AreaUnit = AreaUnitObjects.gigametre_squared
  def `Tm²`: AreaUnit = AreaUnitObjects.terametre_squared
  def Tm2: AreaUnit = AreaUnitObjects.terametre_squared
  def `Pm²`: AreaUnit = AreaUnitObjects.petametre_squared
  def Pm2: AreaUnit = AreaUnitObjects.petametre_squared
  def `Em²`: AreaUnit = AreaUnitObjects.exametre_squared
  def Em2: AreaUnit = AreaUnitObjects.exametre_squared
  def `Zm²`: AreaUnit = AreaUnitObjects.zettametre_squared
  def Zm2: AreaUnit = AreaUnitObjects.zettametre_squared
  def `Ym²`: AreaUnit = AreaUnitObjects.yottametre_squared
  def Ym2: AreaUnit = AreaUnitObjects.yottametre_squared
  def `ft²`: AreaUnit = AreaUnitObjects.foot_squared
  def ft2: AreaUnit = AreaUnitObjects.foot_squared
  def a: AreaUnit = AreaUnitObjects.are
  def ha: AreaUnit = AreaUnitObjects.hectare
}