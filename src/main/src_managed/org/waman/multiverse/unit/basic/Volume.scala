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
  override def getSIUnit: VolumeUnit = VolumeUnitObjects.getSIUnit

}

class DefaultVolumeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeUnit


object VolumeUnitObjects{

  val getSIUnit: VolumeUnit = AreaUnitObjects.getSIUnit * LengthUnitObjects.getSIUnit

  val metre_cubic: VolumeUnit = getSIUnit
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

  def getUnits: Seq[VolumeUnit] =
    Seq(metre_cubic, litre, yoctolitre, zeptolitre, attolitre, femtolitre, picolitre, nanolitre, microlitre, millilitre, centilitre, decilitre, decalitre, hectolitre, kilolitre, megalitre, gigalitre, teralitre, petalitre, exalitre, zettalitre, yottalitre, lambda)
}


object VolumeUnits{
  def `m³`: VolumeUnit = VolumeUnitObjects.metre_cubic
  def m3: VolumeUnit = VolumeUnitObjects.metre_cubic
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

  def getSIUnit: VolumeUnit = VolumeUnitObjects.getSIUnit
  def getUnits: Seq[VolumeUnit] = VolumeUnitObjects.getUnits
}
