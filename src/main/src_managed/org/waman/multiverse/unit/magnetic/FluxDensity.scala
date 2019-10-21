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
  override def getSIUnit: FluxDensityUnit = FluxDensityUnitObjects.getSIUnit

}

class DefaultFluxDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends FluxDensityUnit


object FluxDensityUnitObjects{

  def getSIUnit: FluxDensityUnit = tesla

  final object tesla extends DefaultFluxDensityUnit("tesla", "T", Nil, r"1")
  final object yoctotesla extends DefaultFluxDensityUnit("yoctotesla", "yT", Nil, r"1" * r"1e-24")
  final object zeptotesla extends DefaultFluxDensityUnit("zeptotesla", "zT", Nil, r"1" * r"1e-21")
  final object attotesla extends DefaultFluxDensityUnit("attotesla", "aT", Nil, r"1" * r"1e-18")
  final object femtotesla extends DefaultFluxDensityUnit("femtotesla", "fT", Nil, r"1" * r"1e-15")
  final object picotesla extends DefaultFluxDensityUnit("picotesla", "pT", Nil, r"1" * r"1e-12")
  final object nanotesla extends DefaultFluxDensityUnit("nanotesla", "nT", Nil, r"1" * r"1e-9")
  final object microtesla extends DefaultFluxDensityUnit("microtesla", "μT", Seq("mcT"), r"1" * r"1e-6")
  final object millitesla extends DefaultFluxDensityUnit("millitesla", "mT", Nil, r"1" * r"1e-3")
  final object centitesla extends DefaultFluxDensityUnit("centitesla", "cT", Nil, r"1" * r"1e-2")
  final object decitesla extends DefaultFluxDensityUnit("decitesla", "dT", Nil, r"1" * r"1e-1")
  final object decatesla extends DefaultFluxDensityUnit("decatesla", "daT", Nil, r"1" * r"1e1")
  final object hectotesla extends DefaultFluxDensityUnit("hectotesla", "hT", Nil, r"1" * r"1e2")
  final object kilotesla extends DefaultFluxDensityUnit("kilotesla", "kT", Seq("KT"), r"1" * r"1e3")
  final object megatesla extends DefaultFluxDensityUnit("megatesla", "MT", Nil, r"1" * r"1e6")
  final object gigatesla extends DefaultFluxDensityUnit("gigatesla", "GT", Nil, r"1" * r"1e9")
  final object teratesla extends DefaultFluxDensityUnit("teratesla", "TT", Nil, r"1" * r"1e12")
  final object petatesla extends DefaultFluxDensityUnit("petatesla", "PT", Nil, r"1" * r"1e15")
  final object exatesla extends DefaultFluxDensityUnit("exatesla", "ET", Nil, r"1" * r"1e18")
  final object zettatesla extends DefaultFluxDensityUnit("zettatesla", "ZT", Nil, r"1" * r"1e21")
  final object yottatesla extends DefaultFluxDensityUnit("yottatesla", "YT", Nil, r"1" * r"1e24")
  final object gauss extends DefaultFluxDensityUnit("gauss", "G", Nil, r"1e-4")
  final object yoctogauss extends DefaultFluxDensityUnit("yoctogauss", "yG", Nil, r"1e-4" * r"1e-24")
  final object zeptogauss extends DefaultFluxDensityUnit("zeptogauss", "zG", Nil, r"1e-4" * r"1e-21")
  final object attogauss extends DefaultFluxDensityUnit("attogauss", "aG", Nil, r"1e-4" * r"1e-18")
  final object femtogauss extends DefaultFluxDensityUnit("femtogauss", "fG", Nil, r"1e-4" * r"1e-15")
  final object picogauss extends DefaultFluxDensityUnit("picogauss", "pG", Nil, r"1e-4" * r"1e-12")
  final object nanogauss extends DefaultFluxDensityUnit("nanogauss", "nG", Nil, r"1e-4" * r"1e-9")
  final object microgauss extends DefaultFluxDensityUnit("microgauss", "μG", Seq("mcG"), r"1e-4" * r"1e-6")
  final object milligauss extends DefaultFluxDensityUnit("milligauss", "mG", Nil, r"1e-4" * r"1e-3")
  final object centigauss extends DefaultFluxDensityUnit("centigauss", "cG", Nil, r"1e-4" * r"1e-2")
  final object decigauss extends DefaultFluxDensityUnit("decigauss", "dG", Nil, r"1e-4" * r"1e-1")
  final object decagauss extends DefaultFluxDensityUnit("decagauss", "daG", Nil, r"1e-4" * r"1e1")
  final object hectogauss extends DefaultFluxDensityUnit("hectogauss", "hG", Nil, r"1e-4" * r"1e2")
  final object kilogauss extends DefaultFluxDensityUnit("kilogauss", "kG", Seq("KG"), r"1e-4" * r"1e3")
  final object megagauss extends DefaultFluxDensityUnit("megagauss", "MG", Nil, r"1e-4" * r"1e6")
  final object gigagauss extends DefaultFluxDensityUnit("gigagauss", "GG", Nil, r"1e-4" * r"1e9")
  final object teragauss extends DefaultFluxDensityUnit("teragauss", "TG", Nil, r"1e-4" * r"1e12")
  final object petagauss extends DefaultFluxDensityUnit("petagauss", "PG", Nil, r"1e-4" * r"1e15")
  final object exagauss extends DefaultFluxDensityUnit("exagauss", "EG", Nil, r"1e-4" * r"1e18")
  final object zettagauss extends DefaultFluxDensityUnit("zettagauss", "ZG", Nil, r"1e-4" * r"1e21")
  final object yottagauss extends DefaultFluxDensityUnit("yottagauss", "YG", Nil, r"1e-4" * r"1e24")

  def getUnits: Seq[FluxDensityUnit] =
    Seq(tesla, yoctotesla, zeptotesla, attotesla, femtotesla, picotesla, nanotesla, microtesla, millitesla, centitesla, decitesla, decatesla, hectotesla, kilotesla, megatesla, gigatesla, teratesla, petatesla, exatesla, zettatesla, yottatesla, gauss, yoctogauss, zeptogauss, attogauss, femtogauss, picogauss, nanogauss, microgauss, milligauss, centigauss, decigauss, decagauss, hectogauss, kilogauss, megagauss, gigagauss, teragauss, petagauss, exagauss, zettagauss, yottagauss)
}


object FluxDensityUnits{
  def T: FluxDensityUnit = FluxDensityUnitObjects.tesla
  def yT: FluxDensityUnit = FluxDensityUnitObjects.yoctotesla
  def zT: FluxDensityUnit = FluxDensityUnitObjects.zeptotesla
  def aT: FluxDensityUnit = FluxDensityUnitObjects.attotesla
  def fT: FluxDensityUnit = FluxDensityUnitObjects.femtotesla
  def pT: FluxDensityUnit = FluxDensityUnitObjects.picotesla
  def nT: FluxDensityUnit = FluxDensityUnitObjects.nanotesla
  def μT: FluxDensityUnit = FluxDensityUnitObjects.microtesla
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
  def μG: FluxDensityUnit = FluxDensityUnitObjects.microgauss
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

  def getSIUnit: FluxDensityUnit = FluxDensityUnitObjects.getSIUnit
  def getUnits: Seq[FluxDensityUnit] = FluxDensityUnitObjects.getUnits
}
