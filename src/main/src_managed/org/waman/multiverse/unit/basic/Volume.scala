package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._
class Volume[A: Fractional](val value: A, val unit: VolumeUnit)
    extends LinearQuantity[Volume[A], A, VolumeUnit] {

  override protected def newQuantity(value: A, unit: VolumeUnit): Volume[A] = new Volume(value, unit)
           }

trait VolumeUnit extends LinearUnit[VolumeUnit]{
  override def getSIUnit: VolumeUnit = VolumeUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VolumeUnit.dimension

}

object VolumeUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 3).withDefaultValue(0)

  val getSIUnit: VolumeUnit = AreaUnit.getSIUnit * LengthUnit.getSIUnit

import VolumeUnitObjects._
  def getUnits: Seq[VolumeUnit] =
    Seq(metre_cubic, yoctometre_cubic, zeptometre_cubic, attometre_cubic, femtometre_cubic, picometre_cubic, nanometre_cubic, micrometre_cubic, millimetre_cubic, centimetre_cubic, decimetre_cubic, decametre_cubic, hectometre_cubic, kilometre_cubic, megametre_cubic, gigametre_cubic, terametre_cubic, petametre_cubic, exametre_cubic, zettametre_cubic, yottametre_cubic, litre, yoctolitre, zeptolitre, attolitre, femtolitre, picolitre, nanolitre, microlitre, millilitre, centilitre, decilitre, decalitre, hectolitre, kilolitre, megalitre, gigalitre, teralitre, petalitre, exalitre, zettalitre, yottalitre, lambda)
}



class DefaultVolumeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeUnit

object VolumeUnitObjects{

  val metre_cubic: VolumeUnit = LengthUnitObjects.metre.cubic
  val yoctometre_cubic: VolumeUnit = LengthUnitObjects.yoctometre.cubic
  val zeptometre_cubic: VolumeUnit = LengthUnitObjects.zeptometre.cubic
  val attometre_cubic: VolumeUnit = LengthUnitObjects.attometre.cubic
  val femtometre_cubic: VolumeUnit = LengthUnitObjects.femtometre.cubic
  val picometre_cubic: VolumeUnit = LengthUnitObjects.picometre.cubic
  val nanometre_cubic: VolumeUnit = LengthUnitObjects.nanometre.cubic
  val micrometre_cubic: VolumeUnit = LengthUnitObjects.micrometre.cubic
  val millimetre_cubic: VolumeUnit = LengthUnitObjects.millimetre.cubic
  val centimetre_cubic: VolumeUnit = LengthUnitObjects.centimetre.cubic
  val decimetre_cubic: VolumeUnit = LengthUnitObjects.decimetre.cubic
  val decametre_cubic: VolumeUnit = LengthUnitObjects.decametre.cubic
  val hectometre_cubic: VolumeUnit = LengthUnitObjects.hectometre.cubic
  val kilometre_cubic: VolumeUnit = LengthUnitObjects.kilometre.cubic
  val megametre_cubic: VolumeUnit = LengthUnitObjects.megametre.cubic
  val gigametre_cubic: VolumeUnit = LengthUnitObjects.gigametre.cubic
  val terametre_cubic: VolumeUnit = LengthUnitObjects.terametre.cubic
  val petametre_cubic: VolumeUnit = LengthUnitObjects.petametre.cubic
  val exametre_cubic: VolumeUnit = LengthUnitObjects.exametre.cubic
  val zettametre_cubic: VolumeUnit = LengthUnitObjects.zettametre.cubic
  val yottametre_cubic: VolumeUnit = LengthUnitObjects.yottametre.cubic
  final object litre extends DefaultVolumeUnit("litre", "L", Nil, r"1e-3")
  final object yoctolitre extends DefaultVolumeUnit("yoctolitre", "yL", Nil, r"1e-3" * r"1e-24")
  final object zeptolitre extends DefaultVolumeUnit("zeptolitre", "zL", Nil, r"1e-3" * r"1e-21")
  final object attolitre extends DefaultVolumeUnit("attolitre", "aL", Nil, r"1e-3" * r"1e-18")
  final object femtolitre extends DefaultVolumeUnit("femtolitre", "fL", Nil, r"1e-3" * r"1e-15")
  final object picolitre extends DefaultVolumeUnit("picolitre", "pL", Nil, r"1e-3" * r"1e-12")
  final object nanolitre extends DefaultVolumeUnit("nanolitre", "nL", Nil, r"1e-3" * r"1e-9")
  final object microlitre extends DefaultVolumeUnit("microlitre", "μL", Seq("mcL"), r"1e-3" * r"1e-6")
  final object millilitre extends DefaultVolumeUnit("millilitre", "mL", Nil, r"1e-3" * r"1e-3")
  final object centilitre extends DefaultVolumeUnit("centilitre", "cL", Nil, r"1e-3" * r"1e-2")
  final object decilitre extends DefaultVolumeUnit("decilitre", "dL", Nil, r"1e-3" * r"1e-1")
  final object decalitre extends DefaultVolumeUnit("decalitre", "daL", Nil, r"1e-3" * r"1e1")
  final object hectolitre extends DefaultVolumeUnit("hectolitre", "hL", Nil, r"1e-3" * r"1e2")
  final object kilolitre extends DefaultVolumeUnit("kilolitre", "kL", Seq("KL"), r"1e-3" * r"1e3")
  final object megalitre extends DefaultVolumeUnit("megalitre", "ML", Nil, r"1e-3" * r"1e6")
  final object gigalitre extends DefaultVolumeUnit("gigalitre", "GL", Nil, r"1e-3" * r"1e9")
  final object teralitre extends DefaultVolumeUnit("teralitre", "TL", Nil, r"1e-3" * r"1e12")
  final object petalitre extends DefaultVolumeUnit("petalitre", "PL", Nil, r"1e-3" * r"1e15")
  final object exalitre extends DefaultVolumeUnit("exalitre", "EL", Nil, r"1e-3" * r"1e18")
  final object zettalitre extends DefaultVolumeUnit("zettalitre", "ZL", Nil, r"1e-3" * r"1e21")
  final object yottalitre extends DefaultVolumeUnit("yottalitre", "YL", Nil, r"1e-3" * r"1e24")
  final object lambda extends DefaultVolumeUnit("lambda", "λ", Nil, r"1e-9")
}

object VolumeUnits{
  def `m³`: VolumeUnit = VolumeUnitObjects.metre_cubic
  def m3: VolumeUnit = VolumeUnitObjects.metre_cubic
  def `ym³`: VolumeUnit = VolumeUnitObjects.yoctometre_cubic
  def ym3: VolumeUnit = VolumeUnitObjects.yoctometre_cubic
  def `zm³`: VolumeUnit = VolumeUnitObjects.zeptometre_cubic
  def zm3: VolumeUnit = VolumeUnitObjects.zeptometre_cubic
  def `am³`: VolumeUnit = VolumeUnitObjects.attometre_cubic
  def am3: VolumeUnit = VolumeUnitObjects.attometre_cubic
  def `fm³`: VolumeUnit = VolumeUnitObjects.femtometre_cubic
  def fm3: VolumeUnit = VolumeUnitObjects.femtometre_cubic
  def `pm³`: VolumeUnit = VolumeUnitObjects.picometre_cubic
  def pm3: VolumeUnit = VolumeUnitObjects.picometre_cubic
  def `nm³`: VolumeUnit = VolumeUnitObjects.nanometre_cubic
  def nm3: VolumeUnit = VolumeUnitObjects.nanometre_cubic
  def `μm³`: VolumeUnit = VolumeUnitObjects.micrometre_cubic
  def μm3: VolumeUnit = VolumeUnitObjects.micrometre_cubic
  def `mcm³`: VolumeUnit = VolumeUnitObjects.micrometre_cubic
  def mcm3: VolumeUnit = VolumeUnitObjects.micrometre_cubic
  def `mm³`: VolumeUnit = VolumeUnitObjects.millimetre_cubic
  def mm3: VolumeUnit = VolumeUnitObjects.millimetre_cubic
  def `cm³`: VolumeUnit = VolumeUnitObjects.centimetre_cubic
  def cm3: VolumeUnit = VolumeUnitObjects.centimetre_cubic
  def `dm³`: VolumeUnit = VolumeUnitObjects.decimetre_cubic
  def dm3: VolumeUnit = VolumeUnitObjects.decimetre_cubic
  def `dam³`: VolumeUnit = VolumeUnitObjects.decametre_cubic
  def dam3: VolumeUnit = VolumeUnitObjects.decametre_cubic
  def `hm³`: VolumeUnit = VolumeUnitObjects.hectometre_cubic
  def hm3: VolumeUnit = VolumeUnitObjects.hectometre_cubic
  def `km³`: VolumeUnit = VolumeUnitObjects.kilometre_cubic
  def km3: VolumeUnit = VolumeUnitObjects.kilometre_cubic
  def `Km³`: VolumeUnit = VolumeUnitObjects.kilometre_cubic
  def Km3: VolumeUnit = VolumeUnitObjects.kilometre_cubic
  def `Mm³`: VolumeUnit = VolumeUnitObjects.megametre_cubic
  def Mm3: VolumeUnit = VolumeUnitObjects.megametre_cubic
  def `Gm³`: VolumeUnit = VolumeUnitObjects.gigametre_cubic
  def Gm3: VolumeUnit = VolumeUnitObjects.gigametre_cubic
  def `Tm³`: VolumeUnit = VolumeUnitObjects.terametre_cubic
  def Tm3: VolumeUnit = VolumeUnitObjects.terametre_cubic
  def `Pm³`: VolumeUnit = VolumeUnitObjects.petametre_cubic
  def Pm3: VolumeUnit = VolumeUnitObjects.petametre_cubic
  def `Em³`: VolumeUnit = VolumeUnitObjects.exametre_cubic
  def Em3: VolumeUnit = VolumeUnitObjects.exametre_cubic
  def `Zm³`: VolumeUnit = VolumeUnitObjects.zettametre_cubic
  def Zm3: VolumeUnit = VolumeUnitObjects.zettametre_cubic
  def `Ym³`: VolumeUnit = VolumeUnitObjects.yottametre_cubic
  def Ym3: VolumeUnit = VolumeUnitObjects.yottametre_cubic
  def L: VolumeUnit = VolumeUnitObjects.litre
  def yL: VolumeUnit = VolumeUnitObjects.yoctolitre
  def zL: VolumeUnit = VolumeUnitObjects.zeptolitre
  def aL: VolumeUnit = VolumeUnitObjects.attolitre
  def fL: VolumeUnit = VolumeUnitObjects.femtolitre
  def pL: VolumeUnit = VolumeUnitObjects.picolitre
  def nL: VolumeUnit = VolumeUnitObjects.nanolitre
  def μL: VolumeUnit = VolumeUnitObjects.microlitre
  def mcL: VolumeUnit = VolumeUnitObjects.microlitre
  def mL: VolumeUnit = VolumeUnitObjects.millilitre
  def cL: VolumeUnit = VolumeUnitObjects.centilitre
  def dL: VolumeUnit = VolumeUnitObjects.decilitre
  def daL: VolumeUnit = VolumeUnitObjects.decalitre
  def hL: VolumeUnit = VolumeUnitObjects.hectolitre
  def kL: VolumeUnit = VolumeUnitObjects.kilolitre
  def KL: VolumeUnit = VolumeUnitObjects.kilolitre
  def ML: VolumeUnit = VolumeUnitObjects.megalitre
  def GL: VolumeUnit = VolumeUnitObjects.gigalitre
  def TL: VolumeUnit = VolumeUnitObjects.teralitre
  def PL: VolumeUnit = VolumeUnitObjects.petalitre
  def EL: VolumeUnit = VolumeUnitObjects.exalitre
  def ZL: VolumeUnit = VolumeUnitObjects.zettalitre
  def YL: VolumeUnit = VolumeUnitObjects.yottalitre
  def λ: VolumeUnit = VolumeUnitObjects.lambda
}