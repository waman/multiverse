package org.waman.multiverse.unit.luminous

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class Illuminance[A: Fractional](val value: A, val unit: IlluminanceUnit)
    extends LinearQuantity[Illuminance[A], A, IlluminanceUnit] {

  override protected def newQuantity(value: A, unit: IlluminanceUnit): Illuminance[A] = new Illuminance(value, unit)
}

trait IlluminanceUnit extends LinearUnit[IlluminanceUnit]{
  override def getSIUnit: IlluminanceUnit = IlluminanceUnitObjects.getSIUnit

}

class DefaultIlluminanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends IlluminanceUnit


object IlluminanceUnitObjects{
  final object lux extends DefaultIlluminanceUnit("lux", "lx", Nil, r"1")
  final object yoctolux extends DefaultIlluminanceUnit("yoctolux", "ylx", Nil, r"1" * r"1e-24")
  final object zeptolux extends DefaultIlluminanceUnit("zeptolux", "zlx", Nil, r"1" * r"1e-21")
  final object attolux extends DefaultIlluminanceUnit("attolux", "alx", Nil, r"1" * r"1e-18")
  final object femtolux extends DefaultIlluminanceUnit("femtolux", "flx", Nil, r"1" * r"1e-15")
  final object picolux extends DefaultIlluminanceUnit("picolux", "plx", Nil, r"1" * r"1e-12")
  final object nanolux extends DefaultIlluminanceUnit("nanolux", "nlx", Nil, r"1" * r"1e-9")
  final object microlux extends DefaultIlluminanceUnit("microlux", "μlx", Seq("mclx"), r"1" * r"1e-6")
  final object millilux extends DefaultIlluminanceUnit("millilux", "mlx", Nil, r"1" * r"1e-3")
  final object centilux extends DefaultIlluminanceUnit("centilux", "clx", Nil, r"1" * r"1e-2")
  final object decilux extends DefaultIlluminanceUnit("decilux", "dlx", Nil, r"1" * r"1e-1")
  final object decalux extends DefaultIlluminanceUnit("decalux", "dalx", Nil, r"1" * r"1e1")
  final object hectolux extends DefaultIlluminanceUnit("hectolux", "hlx", Nil, r"1" * r"1e2")
  final object kilolux extends DefaultIlluminanceUnit("kilolux", "klx", Nil, r"1" * r"1e3")
  final object megalux extends DefaultIlluminanceUnit("megalux", "Mlx", Nil, r"1" * r"1e6")
  final object gigalux extends DefaultIlluminanceUnit("gigalux", "Glx", Nil, r"1" * r"1e9")
  final object teralux extends DefaultIlluminanceUnit("teralux", "Tlx", Nil, r"1" * r"1e12")
  final object petalux extends DefaultIlluminanceUnit("petalux", "Plx", Nil, r"1" * r"1e15")
  final object exalux extends DefaultIlluminanceUnit("exalux", "Elx", Nil, r"1" * r"1e18")
  final object zettalux extends DefaultIlluminanceUnit("zettalux", "Zlx", Nil, r"1" * r"1e21")
  final object yottalux extends DefaultIlluminanceUnit("yottalux", "Ylx", Nil, r"1" * r"1e24")
  final object phot extends DefaultIlluminanceUnit("phot", "ph", Nil, r"1e4")


  def getSIUnit: IlluminanceUnit = lux

  def getUnits: Seq[IlluminanceUnit] =
    Seq(lux, yoctolux, zeptolux, attolux, femtolux, picolux, nanolux, microlux, millilux, centilux, decilux, decalux, hectolux, kilolux, megalux, gigalux, teralux, petalux, exalux, zettalux, yottalux, phot)
}

object IlluminanceUnits{
  def lx: IlluminanceUnit = IlluminanceUnitObjects.lux
  def ylx: IlluminanceUnit = IlluminanceUnitObjects.yoctolux
  def zlx: IlluminanceUnit = IlluminanceUnitObjects.zeptolux
  def alx: IlluminanceUnit = IlluminanceUnitObjects.attolux
  def flx: IlluminanceUnit = IlluminanceUnitObjects.femtolux
  def plx: IlluminanceUnit = IlluminanceUnitObjects.picolux
  def nlx: IlluminanceUnit = IlluminanceUnitObjects.nanolux
  def μlx: IlluminanceUnit = IlluminanceUnitObjects.microlux
  def mclx: IlluminanceUnit = IlluminanceUnitObjects.microlux
  def mlx: IlluminanceUnit = IlluminanceUnitObjects.millilux
  def clx: IlluminanceUnit = IlluminanceUnitObjects.centilux
  def dlx: IlluminanceUnit = IlluminanceUnitObjects.decilux
  def dalx: IlluminanceUnit = IlluminanceUnitObjects.decalux
  def hlx: IlluminanceUnit = IlluminanceUnitObjects.hectolux
  def klx: IlluminanceUnit = IlluminanceUnitObjects.kilolux
  def Mlx: IlluminanceUnit = IlluminanceUnitObjects.megalux
  def Glx: IlluminanceUnit = IlluminanceUnitObjects.gigalux
  def Tlx: IlluminanceUnit = IlluminanceUnitObjects.teralux
  def Plx: IlluminanceUnit = IlluminanceUnitObjects.petalux
  def Elx: IlluminanceUnit = IlluminanceUnitObjects.exalux
  def Zlx: IlluminanceUnit = IlluminanceUnitObjects.zettalux
  def Ylx: IlluminanceUnit = IlluminanceUnitObjects.yottalux
  def ph: IlluminanceUnit = IlluminanceUnitObjects.phot

  def getSIUnit: IlluminanceUnit = IlluminanceUnitObjects.getSIUnit
  def getUnits: Seq[IlluminanceUnit] = IlluminanceUnitObjects.getUnits
}
