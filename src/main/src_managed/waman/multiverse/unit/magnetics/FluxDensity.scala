package waman.multiverse.unit.magnetics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


class FluxDensity[A: Fractional](val value: A, val unit: FluxDensityUnit)
    extends LinearQuantity[FluxDensity[A], A, FluxDensityUnit] {

  override protected def newQuantity(value: A, unit: FluxDensityUnit): FluxDensity[A] = new FluxDensity(value, unit)
}

trait FluxDensityUnit extends LinearUnit[FluxDensityUnit]{

  override def getSIUnit: FluxDensityUnit = FluxDensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = FluxDensityUnit.dimension
}

object FluxDensityUnit extends UnitInfo[FluxDensityUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, I -> -1).withDefaultValue(0)

  def getSIUnit: FluxDensityUnit = FluxDensityUnitObjects.tesla

  import FluxDensityUnitObjects._
  def getUnits: Seq[FluxDensityUnit] =
    Seq(tesla, yoctotesla, zeptotesla, attotesla, femtotesla, picotesla, nanotesla, microtesla, millitesla, centitesla, decitesla, decatesla, hectotesla, kilotesla, megatesla, gigatesla, teratesla, petatesla, exatesla, zettatesla, yottatesla, gauss, yoctogauss, zeptogauss, attogauss, femtogauss, picogauss, nanogauss, microgauss, milligauss, centigauss, decigauss, decagauss, hectogauss, kilogauss, megagauss, gigagauss, teragauss, petagauss, exagauss, zettagauss, yottagauss)
}

/** For no aliase or user defined units */
class SimpleFluxDensityUnit(val name: String, val symbol: String, val interval: Real) extends FluxDensityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultFluxDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends FluxDensityUnit

object FluxDensityUnitObjects{

  import spire.implicits._


  final case object tesla extends SimpleFluxDensityUnit("tesla", "T", 1)
  final case object yoctotesla extends SimpleFluxDensityUnit("yoctotesla", "yT", r"1e-24")
  final case object zeptotesla extends SimpleFluxDensityUnit("zeptotesla", "zT", r"1e-21")
  final case object attotesla extends SimpleFluxDensityUnit("attotesla", "aT", r"1e-18")
  final case object femtotesla extends SimpleFluxDensityUnit("femtotesla", "fT", r"1e-15")
  final case object picotesla extends SimpleFluxDensityUnit("picotesla", "pT", r"1e-12")
  final case object nanotesla extends SimpleFluxDensityUnit("nanotesla", "nT", r"1e-9")
  final case object microtesla extends DefaultFluxDensityUnit("microtesla", "μT", Seq("mcT"), r"1e-6")
  final case object millitesla extends SimpleFluxDensityUnit("millitesla", "mT", r"1e-3")
  final case object centitesla extends SimpleFluxDensityUnit("centitesla", "cT", r"1e-2")
  final case object decitesla extends SimpleFluxDensityUnit("decitesla", "dT", r"1e-1")
  final case object decatesla extends SimpleFluxDensityUnit("decatesla", "daT", r"1e1")
  final case object hectotesla extends SimpleFluxDensityUnit("hectotesla", "hT", r"1e2")
  final case object kilotesla extends DefaultFluxDensityUnit("kilotesla", "kT", Seq("KT"), r"1e3")
  final case object megatesla extends SimpleFluxDensityUnit("megatesla", "MT", r"1e6")
  final case object gigatesla extends SimpleFluxDensityUnit("gigatesla", "GT", r"1e9")
  final case object teratesla extends SimpleFluxDensityUnit("teratesla", "TT", r"1e12")
  final case object petatesla extends SimpleFluxDensityUnit("petatesla", "PT", r"1e15")
  final case object exatesla extends SimpleFluxDensityUnit("exatesla", "ET", r"1e18")
  final case object zettatesla extends SimpleFluxDensityUnit("zettatesla", "ZT", r"1e21")
  final case object yottatesla extends SimpleFluxDensityUnit("yottatesla", "YT", r"1e24")
  final case object gauss extends SimpleFluxDensityUnit("gauss", "G", r"1e-4")
  final case object yoctogauss extends SimpleFluxDensityUnit("yoctogauss", "yG", r"1e-4" * r"1e-24")
  final case object zeptogauss extends SimpleFluxDensityUnit("zeptogauss", "zG", r"1e-4" * r"1e-21")
  final case object attogauss extends SimpleFluxDensityUnit("attogauss", "aG", r"1e-4" * r"1e-18")
  final case object femtogauss extends SimpleFluxDensityUnit("femtogauss", "fG", r"1e-4" * r"1e-15")
  final case object picogauss extends SimpleFluxDensityUnit("picogauss", "pG", r"1e-4" * r"1e-12")
  final case object nanogauss extends SimpleFluxDensityUnit("nanogauss", "nG", r"1e-4" * r"1e-9")
  final case object microgauss extends DefaultFluxDensityUnit("microgauss", "μG", Seq("mcG"), r"1e-4" * r"1e-6")
  final case object milligauss extends SimpleFluxDensityUnit("milligauss", "mG", r"1e-4" * r"1e-3")
  final case object centigauss extends SimpleFluxDensityUnit("centigauss", "cG", r"1e-4" * r"1e-2")
  final case object decigauss extends SimpleFluxDensityUnit("decigauss", "dG", r"1e-4" * r"1e-1")
  final case object decagauss extends SimpleFluxDensityUnit("decagauss", "daG", r"1e-4" * r"1e1")
  final case object hectogauss extends SimpleFluxDensityUnit("hectogauss", "hG", r"1e-4" * r"1e2")
  final case object kilogauss extends DefaultFluxDensityUnit("kilogauss", "kG", Seq("KG"), r"1e-4" * r"1e3")
  final case object megagauss extends SimpleFluxDensityUnit("megagauss", "MG", r"1e-4" * r"1e6")
  final case object gigagauss extends SimpleFluxDensityUnit("gigagauss", "GG", r"1e-4" * r"1e9")
  final case object teragauss extends SimpleFluxDensityUnit("teragauss", "TG", r"1e-4" * r"1e12")
  final case object petagauss extends SimpleFluxDensityUnit("petagauss", "PG", r"1e-4" * r"1e15")
  final case object exagauss extends SimpleFluxDensityUnit("exagauss", "EG", r"1e-4" * r"1e18")
  final case object zettagauss extends SimpleFluxDensityUnit("zettagauss", "ZG", r"1e-4" * r"1e21")
  final case object yottagauss extends SimpleFluxDensityUnit("yottagauss", "YG", r"1e-4" * r"1e24")
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