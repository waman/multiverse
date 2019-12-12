package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.fluid.VolumeFlow
import org.waman.multiverse.unit.fluid.VolumeFlowUnit

class Volume[A: Fractional](val value: A, val unit: VolumeUnit)
    extends LinearQuantity[Volume[A], A, VolumeUnit] {

  override protected def newQuantity(value: A, unit: VolumeUnit): Volume[A] = new Volume(value, unit)
  def /(time: Time[A]): VolumeFlow[A] = new VolumeFlow(this.value / time.value, this.unit / time.unit)

}

trait VolumeUnit extends LinearUnit[VolumeUnit]{

  override def getSIUnit: VolumeUnit = VolumeUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VolumeUnit.dimension

  def /(timeUnit: TimeUnit): VolumeFlowUnit =
    new QuotientUnit[VolumeFlowUnit, VolumeUnit, TimeUnit](VolumeUnit.this, timeUnit) with VolumeFlowUnit

}

object VolumeUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 3).withDefaultValue(0)

  val getSIUnit: VolumeUnit = AreaUnit.getSIUnit * LengthUnit.getSIUnit

  import VolumeUnitObjects._
  def getUnits: Seq[VolumeUnit] =
    Seq(cubic_metre, cubic_yoctometre, cubic_zeptometre, cubic_attometre, cubic_femtometre, cubic_picometre, cubic_nanometre, cubic_micrometre, cubic_millimetre, cubic_centimetre, cubic_decimetre, cubic_decametre, cubic_hectometre, cubic_kilometre, cubic_megametre, cubic_gigametre, cubic_terametre, cubic_petametre, cubic_exametre, cubic_zettametre, cubic_yottametre, cubic_foot, cubic_inch, litre, yoctolitre, zeptolitre, attolitre, femtolitre, picolitre, nanolitre, microlitre, millilitre, centilitre, decilitre, decalitre, hectolitre, kilolitre, megalitre, gigalitre, teralitre, petalitre, exalitre, zettalitre, yottalitre, lambda, gallon, `gallon(US_fluid)`)
}


sealed trait gallonAttribute

object VolumeAttributes{
  final object US_fluid extends gallonAttribute
}

class DefaultVolumeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeUnit

object VolumeUnitObjects{

  val cubic_metre: VolumeUnit = LengthUnitObjects.metre.cubic
  val cubic_yoctometre: VolumeUnit = LengthUnitObjects.yoctometre.cubic
  val cubic_zeptometre: VolumeUnit = LengthUnitObjects.zeptometre.cubic
  val cubic_attometre: VolumeUnit = LengthUnitObjects.attometre.cubic
  val cubic_femtometre: VolumeUnit = LengthUnitObjects.femtometre.cubic
  val cubic_picometre: VolumeUnit = LengthUnitObjects.picometre.cubic
  val cubic_nanometre: VolumeUnit = LengthUnitObjects.nanometre.cubic
  val cubic_micrometre: VolumeUnit = LengthUnitObjects.micrometre.cubic
  val cubic_millimetre: VolumeUnit = LengthUnitObjects.millimetre.cubic
  val cubic_centimetre: VolumeUnit = LengthUnitObjects.centimetre.cubic
  val cubic_decimetre: VolumeUnit = LengthUnitObjects.decimetre.cubic
  val cubic_decametre: VolumeUnit = LengthUnitObjects.decametre.cubic
  val cubic_hectometre: VolumeUnit = LengthUnitObjects.hectometre.cubic
  val cubic_kilometre: VolumeUnit = LengthUnitObjects.kilometre.cubic
  val cubic_megametre: VolumeUnit = LengthUnitObjects.megametre.cubic
  val cubic_gigametre: VolumeUnit = LengthUnitObjects.gigametre.cubic
  val cubic_terametre: VolumeUnit = LengthUnitObjects.terametre.cubic
  val cubic_petametre: VolumeUnit = LengthUnitObjects.petametre.cubic
  val cubic_exametre: VolumeUnit = LengthUnitObjects.exametre.cubic
  val cubic_zettametre: VolumeUnit = LengthUnitObjects.zettametre.cubic
  val cubic_yottametre: VolumeUnit = LengthUnitObjects.yottametre.cubic
  val cubic_foot: VolumeUnit = LengthUnitObjects.foot.cubic
  val cubic_inch: VolumeUnit = LengthUnitObjects.inch.cubic
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
  final object gallon extends DefaultVolumeUnit("gallon", "gal", Nil, `gallon(US_fluid)`.interval)
  final object `gallon(US_fluid)` extends DefaultVolumeUnit("gallon(US_fluid)", "gal(US_fluid)", Nil, r"231" * cubic_inch.interval)
}

object VolumeUnits{
  def `m³`: VolumeUnit = VolumeUnitObjects.cubic_metre
  def m3: VolumeUnit = VolumeUnitObjects.cubic_metre
  def `ym³`: VolumeUnit = VolumeUnitObjects.cubic_yoctometre
  def ym3: VolumeUnit = VolumeUnitObjects.cubic_yoctometre
  def `zm³`: VolumeUnit = VolumeUnitObjects.cubic_zeptometre
  def zm3: VolumeUnit = VolumeUnitObjects.cubic_zeptometre
  def `am³`: VolumeUnit = VolumeUnitObjects.cubic_attometre
  def am3: VolumeUnit = VolumeUnitObjects.cubic_attometre
  def `fm³`: VolumeUnit = VolumeUnitObjects.cubic_femtometre
  def fm3: VolumeUnit = VolumeUnitObjects.cubic_femtometre
  def `pm³`: VolumeUnit = VolumeUnitObjects.cubic_picometre
  def pm3: VolumeUnit = VolumeUnitObjects.cubic_picometre
  def `nm³`: VolumeUnit = VolumeUnitObjects.cubic_nanometre
  def nm3: VolumeUnit = VolumeUnitObjects.cubic_nanometre
  def `μm³`: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def μm3: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def `mcm³`: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def mcm3: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def `mm³`: VolumeUnit = VolumeUnitObjects.cubic_millimetre
  def mm3: VolumeUnit = VolumeUnitObjects.cubic_millimetre
  def `cm³`: VolumeUnit = VolumeUnitObjects.cubic_centimetre
  def cm3: VolumeUnit = VolumeUnitObjects.cubic_centimetre
  def `dm³`: VolumeUnit = VolumeUnitObjects.cubic_decimetre
  def dm3: VolumeUnit = VolumeUnitObjects.cubic_decimetre
  def `dam³`: VolumeUnit = VolumeUnitObjects.cubic_decametre
  def dam3: VolumeUnit = VolumeUnitObjects.cubic_decametre
  def `hm³`: VolumeUnit = VolumeUnitObjects.cubic_hectometre
  def hm3: VolumeUnit = VolumeUnitObjects.cubic_hectometre
  def `km³`: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def km3: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def `Km³`: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def Km3: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def `Mm³`: VolumeUnit = VolumeUnitObjects.cubic_megametre
  def Mm3: VolumeUnit = VolumeUnitObjects.cubic_megametre
  def `Gm³`: VolumeUnit = VolumeUnitObjects.cubic_gigametre
  def Gm3: VolumeUnit = VolumeUnitObjects.cubic_gigametre
  def `Tm³`: VolumeUnit = VolumeUnitObjects.cubic_terametre
  def Tm3: VolumeUnit = VolumeUnitObjects.cubic_terametre
  def `Pm³`: VolumeUnit = VolumeUnitObjects.cubic_petametre
  def Pm3: VolumeUnit = VolumeUnitObjects.cubic_petametre
  def `Em³`: VolumeUnit = VolumeUnitObjects.cubic_exametre
  def Em3: VolumeUnit = VolumeUnitObjects.cubic_exametre
  def `Zm³`: VolumeUnit = VolumeUnitObjects.cubic_zettametre
  def Zm3: VolumeUnit = VolumeUnitObjects.cubic_zettametre
  def `Ym³`: VolumeUnit = VolumeUnitObjects.cubic_yottametre
  def Ym3: VolumeUnit = VolumeUnitObjects.cubic_yottametre
  def `ft³`: VolumeUnit = VolumeUnitObjects.cubic_foot
  def ft3: VolumeUnit = VolumeUnitObjects.cubic_foot
  def `in³`: VolumeUnit = VolumeUnitObjects.cubic_inch
  def in3: VolumeUnit = VolumeUnitObjects.cubic_inch
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
  def gal: VolumeUnit = VolumeUnitObjects.gallon
  def gal(a: gallonAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US_fluid => VolumeUnitObjects.`gallon(US_fluid)`
  }
}