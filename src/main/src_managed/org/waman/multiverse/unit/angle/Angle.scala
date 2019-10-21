package org.waman.multiverse.unit.angle

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit

class Angle[A: Fractional](val value: A, val unit: AngleUnit)
    extends LinearQuantity[Angle[A], A, AngleUnit] {

  override protected def newQuantity(value: A, unit: AngleUnit): Angle[A] = new Angle(value, unit)
           
  def /(time: Time[A]): AngularVelocity[A] = new AngularVelocity(this.value / time.value, this.unit / time.unit)

}

trait AngleUnit extends LinearUnit[AngleUnit]{
  override def getSIUnit: AngleUnit = AngleUnitObjects.getSIUnit


  def /(timeUnit: TimeUnit): AngularVelocityUnit =
    new QuotientUnit[AngularVelocityUnit, AngleUnit, TimeUnit](AngleUnit.this, timeUnit) with AngularVelocityUnit
}

class DefaultAngleUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AngleUnit


object AngleUnitObjects{
  import org.waman.multiverse.unit.Constants


  def getSIUnit: AngleUnit = radian

  final object radian extends DefaultAngleUnit("radian", "rad", Nil, r"1")
  final object degree extends DefaultAngleUnit("degree", "°", Seq("deg"), r"2" * Constants.Pi / r"360")
  final object arcmin extends DefaultAngleUnit("arcmin", "arcmin", Seq("MOA"), r"1"/r"60" * degree.interval)
  final object arcsec extends DefaultAngleUnit("arcsec", "arcsec", Seq("as"), r"1"/r"60" * arcmin.interval)
  final object gradian extends DefaultAngleUnit("gradian", "ᵍ", Seq("grad", "gon"), r"2" * Constants.Pi / r"400")
  final object turn extends DefaultAngleUnit("turn", "tr", Nil, r"2" * Constants.Pi)
  final object sign extends DefaultAngleUnit("sign", "sign", Nil, r"30" * degree.interval)
  final object octant extends DefaultAngleUnit("octant", "octant", Nil, r"45" * degree.interval)
  final object sextant extends DefaultAngleUnit("sextant", "sextant", Nil, r"60" * degree.interval)
  final object quadrant extends DefaultAngleUnit("quadrant", "quadrant", Nil, r"90" * degree.interval)

  def getUnits: Seq[AngleUnit] =
    Seq(radian, degree, arcmin, arcsec, gradian, turn, sign, octant, sextant, quadrant)
}


object AngleUnits{
  def rad: AngleUnit = AngleUnitObjects.radian
  def `°`: AngleUnit = AngleUnitObjects.degree
  def deg: AngleUnit = AngleUnitObjects.degree
  def arcmin: AngleUnit = AngleUnitObjects.arcmin
  def MOA: AngleUnit = AngleUnitObjects.arcmin
  def arcsec: AngleUnit = AngleUnitObjects.arcsec
  def as: AngleUnit = AngleUnitObjects.arcsec
  def ᵍ: AngleUnit = AngleUnitObjects.gradian
  def grad: AngleUnit = AngleUnitObjects.gradian
  def gon: AngleUnit = AngleUnitObjects.gradian
  def tr: AngleUnit = AngleUnitObjects.turn
  def sign: AngleUnit = AngleUnitObjects.sign
  def octant: AngleUnit = AngleUnitObjects.octant
  def sextant: AngleUnit = AngleUnitObjects.sextant
  def quadrant: AngleUnit = AngleUnitObjects.quadrant

  def getSIUnit: AngleUnit = AngleUnitObjects.getSIUnit
  def getUnits: Seq[AngleUnit] = AngleUnitObjects.getUnits
}
