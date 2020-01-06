package org.waman.multiverse.unit.magnetic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class FluxDensity[A: Fractional](val value: A, val unit: FluxDensityUnit)
    extends LinearQuantity[FluxDensity[A], A, FluxDensityUnit] {

  override protected def newQuantity(value: A, unit: FluxDensityUnit): FluxDensity[A] = new FluxDensity(value, unit)

}

trait FluxDensityUnit extends LinearUnit[FluxDensityUnit]{

  override def getSIUnit: FluxDensityUnit = FluxDensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = FluxDensityUnit.dimension

}

/** For user defined units */
class SimpleFluxDensityUnit(val name: String, val symbol: String, val interval: Real) extends FluxDensityUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultFluxDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends FluxDensityUnit

object FluxDensityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, I -> -1).withDefaultValue(0)

  def getSIUnit: FluxDensityUnit = FluxDensityUnitObjects.tesla

  import FluxDensityUnitObjects._
  def getUnits: Seq[FluxDensityUnit] =
    Seq(tesla, yoctotesla, zeptotesla, attotesla, femtotesla, picotesla, nanotesla, microtesla, millitesla, centitesla, decitesla, decatesla, hectotesla, kilotesla, megatesla, gigatesla, teratesla, petatesla, exatesla, zettatesla, yottatesla, gauss, yoctogauss, zeptogauss, attogauss, femtogauss, picogauss, nanogauss, microgauss, milligauss, centigauss, decigauss, decagauss, hectogauss, kilogauss, megagauss, gigagauss, teragauss, petagauss, exagauss, zettagauss, yottagauss)
}

object FluxDensityUnitObjects{

  final case object tesla extends DefaultFluxDensityUnit("tesla", "T", Nil, 1)
  final case object yoctotesla extends DefaultFluxDensityUnit("yoctotesla", "yT", Nil, r"1e-24")
  final case object zeptotesla extends DefaultFluxDensityUnit("zeptotesla", "zT", Nil, r"1e-21")
  final case object attotesla extends DefaultFluxDensityUnit("attotesla", "aT", Nil, r"1e-18")
  final case object femtotesla extends DefaultFluxDensityUnit("femtotesla", "fT", Nil, r"1e-15")
  final case object picotesla extends DefaultFluxDensityUnit("picotesla", "pT", Nil, r"1e-12")
  final case object nanotesla extends DefaultFluxDensityUnit("nanotesla", "nT", Nil, r"1e-9")
  final case object microtesla extends DefaultFluxDensityUnit("microtesla", "μT", Seq("mcT"), r"1e-6")
  final case object millitesla extends DefaultFluxDensityUnit("millitesla", "mT", Nil, r"1e-3")
  final case object centitesla extends DefaultFluxDensityUnit("centitesla", "cT", Nil, r"1e-2")
  final case object decitesla extends DefaultFluxDensityUnit("decitesla", "dT", Nil, r"1e-1")
  final case object decatesla extends DefaultFluxDensityUnit("decatesla", "daT", Nil, r"1e1")
  final case object hectotesla extends DefaultFluxDensityUnit("hectotesla", "hT", Nil, r"1e2")
  final case object kilotesla extends DefaultFluxDensityUnit("kilotesla", "kT", Seq("KT"), r"1e3")
  final case object megatesla extends DefaultFluxDensityUnit("megatesla", "MT", Nil, r"1e6")
  final case object gigatesla extends DefaultFluxDensityUnit("gigatesla", "GT", Nil, r"1e9")
  final case object teratesla extends DefaultFluxDensityUnit("teratesla", "TT", Nil, r"1e12")
  final case object petatesla extends DefaultFluxDensityUnit("petatesla", "PT", Nil, r"1e15")
  final case object exatesla extends DefaultFluxDensityUnit("exatesla", "ET", Nil, r"1e18")
  final case object zettatesla extends DefaultFluxDensityUnit("zettatesla", "ZT", Nil, r"1e21")
  final case object yottatesla extends DefaultFluxDensityUnit("yottatesla", "YT", Nil, r"1e24")
  final case object gauss extends DefaultFluxDensityUnit("gauss", "G", Nil, r"1e-4")
  final case object yoctogauss extends DefaultFluxDensityUnit("yoctogauss", "yG", Nil, r"1e-4" * r"1e-24")
  final case object zeptogauss extends DefaultFluxDensityUnit("zeptogauss", "zG", Nil, r"1e-4" * r"1e-21")
  final case object attogauss extends DefaultFluxDensityUnit("attogauss", "aG", Nil, r"1e-4" * r"1e-18")
  final case object femtogauss extends DefaultFluxDensityUnit("femtogauss", "fG", Nil, r"1e-4" * r"1e-15")
  final case object picogauss extends DefaultFluxDensityUnit("picogauss", "pG", Nil, r"1e-4" * r"1e-12")
  final case object nanogauss extends DefaultFluxDensityUnit("nanogauss", "nG", Nil, r"1e-4" * r"1e-9")
  final case object microgauss extends DefaultFluxDensityUnit("microgauss", "μG", Seq("mcG"), r"1e-4" * r"1e-6")
  final case object milligauss extends DefaultFluxDensityUnit("milligauss", "mG", Nil, r"1e-4" * r"1e-3")
  final case object centigauss extends DefaultFluxDensityUnit("centigauss", "cG", Nil, r"1e-4" * r"1e-2")
  final case object decigauss extends DefaultFluxDensityUnit("decigauss", "dG", Nil, r"1e-4" * r"1e-1")
  final case object decagauss extends DefaultFluxDensityUnit("decagauss", "daG", Nil, r"1e-4" * r"1e1")
  final case object hectogauss extends DefaultFluxDensityUnit("hectogauss", "hG", Nil, r"1e-4" * r"1e2")
  final case object kilogauss extends DefaultFluxDensityUnit("kilogauss", "kG", Seq("KG"), r"1e-4" * r"1e3")
  final case object megagauss extends DefaultFluxDensityUnit("megagauss", "MG", Nil, r"1e-4" * r"1e6")
  final case object gigagauss extends DefaultFluxDensityUnit("gigagauss", "GG", Nil, r"1e-4" * r"1e9")
  final case object teragauss extends DefaultFluxDensityUnit("teragauss", "TG", Nil, r"1e-4" * r"1e12")
  final case object petagauss extends DefaultFluxDensityUnit("petagauss", "PG", Nil, r"1e-4" * r"1e15")
  final case object exagauss extends DefaultFluxDensityUnit("exagauss", "EG", Nil, r"1e-4" * r"1e18")
  final case object zettagauss extends DefaultFluxDensityUnit("zettagauss", "ZG", Nil, r"1e-4" * r"1e21")
  final case object yottagauss extends DefaultFluxDensityUnit("yottagauss", "YG", Nil, r"1e-4" * r"1e24")
}

object FluxDensityUnits{
  def T: FluxDensityUnit = FluxDensityUnitObjects.tesla
  def yT: FluxDensityUnit = FluxDensityUnitObjects.yoctotesla
  def zT: FluxDensityUnit = FluxDensityUnitObjects.zeptotesla
  def aT: FluxDensityUnit = FluxDensityUnitObjects.attotesla
  def fT: FluxDensityUnit = FluxDensityUnitObjects.femtotesla
  def pT: FluxDensityUnit = FluxDensityUnitObjects.picotesla
  def nT: FluxDensityUnit = FluxDensityUnitObjects.nanotesla
  def `μT`: FluxDensityUnit = FluxDensityUnitObjects.microtesla
  def mcT: FluxDensityUnit = FluxDensityUnitObjects.microtesla
  def mT: FluxDensityUnit = FluxDensityUnitObjects.millitesla
  def cT: FluxDensityUnit = FluxDensityUnitObjects.centitesla
  def dT: FluxDensityUnit = FluxDensityUnitObjects.decitesla
  def daT: FluxDensityUnit = FluxDensityUnitObjects.decatesla
  def hT: FluxDensityUnit = FluxDensityUnitObjects.hectotesla
  def kT: FluxDensityUnit = FluxDensityUnitObjects.kilotesla
  def KT: FluxDensityUnit = FluxDensityUnitObjects.kilotesla
  def MT: FluxDensityUnit = FluxDensityUnitObjects.megatesla
  def GT: FluxDensityUnit = FluxDensityUnitObjects.gigatesla
  def TT: FluxDensityUnit = FluxDensityUnitObjects.teratesla
  def PT: FluxDensityUnit = FluxDensityUnitObjects.petatesla
  def ET: FluxDensityUnit = FluxDensityUnitObjects.exatesla
  def ZT: FluxDensityUnit = FluxDensityUnitObjects.zettatesla
  def YT: FluxDensityUnit = FluxDensityUnitObjects.yottatesla
  def G: FluxDensityUnit = FluxDensityUnitObjects.gauss
  def yG: FluxDensityUnit = FluxDensityUnitObjects.yoctogauss
  def zG: FluxDensityUnit = FluxDensityUnitObjects.zeptogauss
  def aG: FluxDensityUnit = FluxDensityUnitObjects.attogauss
  def fG: FluxDensityUnit = FluxDensityUnitObjects.femtogauss
  def pG: FluxDensityUnit = FluxDensityUnitObjects.picogauss
  def nG: FluxDensityUnit = FluxDensityUnitObjects.nanogauss
  def `μG`: FluxDensityUnit = FluxDensityUnitObjects.microgauss
  def mcG: FluxDensityUnit = FluxDensityUnitObjects.microgauss
  def mG: FluxDensityUnit = FluxDensityUnitObjects.milligauss
  def cG: FluxDensityUnit = FluxDensityUnitObjects.centigauss
  def dG: FluxDensityUnit = FluxDensityUnitObjects.decigauss
  def daG: FluxDensityUnit = FluxDensityUnitObjects.decagauss
  def hG: FluxDensityUnit = FluxDensityUnitObjects.hectogauss
  def kG: FluxDensityUnit = FluxDensityUnitObjects.kilogauss
  def KG: FluxDensityUnit = FluxDensityUnitObjects.kilogauss
  def MG: FluxDensityUnit = FluxDensityUnitObjects.megagauss
  def GG: FluxDensityUnit = FluxDensityUnitObjects.gigagauss
  def TG: FluxDensityUnit = FluxDensityUnitObjects.teragauss
  def PG: FluxDensityUnit = FluxDensityUnitObjects.petagauss
  def EG: FluxDensityUnit = FluxDensityUnitObjects.exagauss
  def ZG: FluxDensityUnit = FluxDensityUnitObjects.zettagauss
  def YG: FluxDensityUnit = FluxDensityUnitObjects.yottagauss
}