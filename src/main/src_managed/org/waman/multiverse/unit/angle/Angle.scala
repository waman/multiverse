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

  override def getSIUnit: AngleUnit = AngleUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AngleUnit.dimension

  def /(timeUnit: TimeUnit): AngularVelocityUnit =
    new AbstractQuotientUnit[AngularVelocityUnit, AngleUnit, TimeUnit](AngleUnit.this, timeUnit) with AngularVelocityUnit

}

/** For user defined units */
class SimpleAngleUnit(val name: String, val symbol: String, val interval: Real) extends AngleUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultAngleUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AngleUnit

object AngleUnit{
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int]().withDefaultValue(0)

  def getSIUnit: AngleUnit = AngleUnitObjects.radian

  import AngleUnitObjects._
  def getUnits: Seq[AngleUnit] =
    Seq(radian, degree, arcmin, arcsec, gradian, turn, sign, octant, sextant, quadrant)
}

object AngleUnitObjects{
  import org.waman.multiverse.unit.Constants

  final case object radian extends DefaultAngleUnit("radian", "rad", Nil, 1)
  final case object degree extends DefaultAngleUnit("degree", "°", Seq("deg"), r"2" * Constants.Pi / r"360")
  final case object arcmin extends DefaultAngleUnit("arcmin", "arcmin", Seq("MOA"), r"1"/r"60" * degree.interval)
  final case object arcsec extends DefaultAngleUnit("arcsec", "arcsec", Seq("as"), r"1"/r"60" * arcmin.interval)
  final case object gradian extends DefaultAngleUnit("gradian", "ᵍ", Seq("grad", "gon"), r"2" * Constants.Pi / r"400")
  final case object turn extends DefaultAngleUnit("turn", "tr", Nil, r"2" * Constants.Pi)
  final case object sign extends DefaultAngleUnit("sign", "sign", Nil, r"30" * degree.interval)
  final case object octant extends DefaultAngleUnit("octant", "octant", Nil, r"45" * degree.interval)
  final case object sextant extends DefaultAngleUnit("sextant", "sextant", Nil, r"60" * degree.interval)
  final case object quadrant extends DefaultAngleUnit("quadrant", "quadrant", Nil, r"90" * degree.interval)
}

object AngleUnits{
  def rad: AngleUnit = AngleUnitObjects.radian
  def `°`: AngleUnit = AngleUnitObjects.degree
  def deg: AngleUnit = AngleUnitObjects.degree
  def arcmin: AngleUnit = AngleUnitObjects.arcmin
  def MOA: AngleUnit = AngleUnitObjects.arcmin
  def arcsec: AngleUnit = AngleUnitObjects.arcsec
  def as: AngleUnit = AngleUnitObjects.arcsec
  def `ᵍ`: AngleUnit = AngleUnitObjects.gradian
  def grad: AngleUnit = AngleUnitObjects.gradian
  def gon: AngleUnit = AngleUnitObjects.gradian
  def tr: AngleUnit = AngleUnitObjects.turn
  def sign: AngleUnit = AngleUnitObjects.sign
  def octant: AngleUnit = AngleUnitObjects.octant
  def sextant: AngleUnit = AngleUnitObjects.sextant
  def quadrant: AngleUnit = AngleUnitObjects.quadrant
}