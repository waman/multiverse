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
  override def getSIUnit: AreaUnit = AreaUnitObjects.getSIUnit


  def *(lengthUnit: LengthUnit): VolumeUnit =
    new ProductUnit[VolumeUnit, AreaUnit, LengthUnit](AreaUnit.this, lengthUnit) with VolumeUnit
}

class DefaultAreaUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaUnit


object AreaUnitObjects{

  val getSIUnit: AreaUnit = LengthUnitObjects.getSIUnit * LengthUnitObjects.getSIUnit

  val metre_squared: AreaUnit = LengthUnitObjects.metre.square
  val yoctometre_squared: AreaUnit = LengthUnitObjects.yoctometre.square
  val zeptometre_squared: AreaUnit = LengthUnitObjects.zeptometre.square
  val attometre_squared: AreaUnit = LengthUnitObjects.attometre.square
  val femtometre_squared: AreaUnit = LengthUnitObjects.femtometre.square
  val picometre_squared: AreaUnit = LengthUnitObjects.picometre.square
  val nanometre_squared: AreaUnit = LengthUnitObjects.nanometre.square
  val micrometre_squared: AreaUnit = LengthUnitObjects.micrometre.square
  val millimetre_squared: AreaUnit = LengthUnitObjects.millimetre.square
  val centimetre_squared: AreaUnit = LengthUnitObjects.centimetre.square
  val decimetre_squared: AreaUnit = LengthUnitObjects.decimetre.square
  val decametre_squared: AreaUnit = LengthUnitObjects.decametre.square
  val hectometre_squared: AreaUnit = LengthUnitObjects.hectometre.square
  val kilometre_squared: AreaUnit = LengthUnitObjects.kilometre.square
  val megametre_squared: AreaUnit = LengthUnitObjects.megametre.square
  val gigametre_squared: AreaUnit = LengthUnitObjects.gigametre.square
  val terametre_squared: AreaUnit = LengthUnitObjects.terametre.square
  val petametre_squared: AreaUnit = LengthUnitObjects.petametre.square
  val exametre_squared: AreaUnit = LengthUnitObjects.exametre.square
  val zettametre_squared: AreaUnit = LengthUnitObjects.zettametre.square
  val yottametre_squared: AreaUnit = LengthUnitObjects.yottametre.square
  final object are extends DefaultAreaUnit("are", "a", Nil, r"1e2")
  final object hectare extends DefaultAreaUnit("hectare", "ha", Nil, r"1e4")

  def getUnits: Seq[AreaUnit] =
    Seq(metre_squared, yoctometre_squared, zeptometre_squared, attometre_squared, femtometre_squared, picometre_squared, nanometre_squared, micrometre_squared, millimetre_squared, centimetre_squared, decimetre_squared, decametre_squared, hectometre_squared, kilometre_squared, megametre_squared, gigametre_squared, terametre_squared, petametre_squared, exametre_squared, zettametre_squared, yottametre_squared, are, hectare)
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
  def a: AreaUnit = AreaUnitObjects.are
  def ha: AreaUnit = AreaUnitObjects.hectare

  def getSIUnit: AreaUnit = AreaUnitObjects.getSIUnit
  def getUnits: Seq[AreaUnit] = AreaUnitObjects.getUnits
}
