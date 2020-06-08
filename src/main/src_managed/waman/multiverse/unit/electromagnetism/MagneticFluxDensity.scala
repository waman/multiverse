package waman.multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


class MagneticFluxDensity[A: Fractional](val value: A, val unit: MagneticFluxDensityUnit)
    extends LinearQuantity[MagneticFluxDensity[A], A, MagneticFluxDensityUnit] {

  override protected def newQuantity(value: A, unit: MagneticFluxDensityUnit): MagneticFluxDensity[A] = new MagneticFluxDensity(value, unit)
}

trait MagneticFluxDensityUnit extends LinearUnit[MagneticFluxDensityUnit]{

  override def getSIUnit: MagneticFluxDensityUnit = MagneticFluxDensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MagneticFluxDensityUnit.dimension
}

object MagneticFluxDensityUnit extends UnitInfo[MagneticFluxDensityUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, I -> -1).withDefaultValue(0)

  def getSIUnit: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.tesla

  import MagneticFluxDensityUnitObjects._
  def getUnits: Seq[MagneticFluxDensityUnit] =
    Seq(tesla, yoctotesla, zeptotesla, attotesla, femtotesla, picotesla, nanotesla, microtesla, millitesla, centitesla, decitesla, decatesla, hectotesla, kilotesla, megatesla, gigatesla, teratesla, petatesla, exatesla, zettatesla, yottatesla, gauss, yoctogauss, zeptogauss, attogauss, femtogauss, picogauss, nanogauss, microgauss, milligauss, centigauss, decigauss, decagauss, hectogauss, kilogauss, megagauss, gigagauss, teragauss, petagauss, exagauss, zettagauss, yottagauss)
}

/** For no aliase or user defined units */
class SimpleMagneticFluxDensityUnit(val name: String, val symbol: String, val interval: Real) extends MagneticFluxDensityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultMagneticFluxDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MagneticFluxDensityUnit

object MagneticFluxDensityUnitObjects{

  import spire.implicits._


  final case object tesla extends SimpleMagneticFluxDensityUnit("tesla", "T", 1)
  final case object yoctotesla extends SimpleMagneticFluxDensityUnit("yoctotesla", "yT", r"1e-24")
  final case object zeptotesla extends SimpleMagneticFluxDensityUnit("zeptotesla", "zT", r"1e-21")
  final case object attotesla extends SimpleMagneticFluxDensityUnit("attotesla", "aT", r"1e-18")
  final case object femtotesla extends SimpleMagneticFluxDensityUnit("femtotesla", "fT", r"1e-15")
  final case object picotesla extends SimpleMagneticFluxDensityUnit("picotesla", "pT", r"1e-12")
  final case object nanotesla extends SimpleMagneticFluxDensityUnit("nanotesla", "nT", r"1e-9")
  final case object microtesla extends DefaultMagneticFluxDensityUnit("microtesla", "μT", Seq("mcT"), r"1e-6")
  final case object millitesla extends SimpleMagneticFluxDensityUnit("millitesla", "mT", r"1e-3")
  final case object centitesla extends SimpleMagneticFluxDensityUnit("centitesla", "cT", r"1e-2")
  final case object decitesla extends SimpleMagneticFluxDensityUnit("decitesla", "dT", r"1e-1")
  final case object decatesla extends SimpleMagneticFluxDensityUnit("decatesla", "daT", r"1e1")
  final case object hectotesla extends SimpleMagneticFluxDensityUnit("hectotesla", "hT", r"1e2")
  final case object kilotesla extends DefaultMagneticFluxDensityUnit("kilotesla", "kT", Seq("KT"), r"1e3")
  final case object megatesla extends SimpleMagneticFluxDensityUnit("megatesla", "MT", r"1e6")
  final case object gigatesla extends SimpleMagneticFluxDensityUnit("gigatesla", "GT", r"1e9")
  final case object teratesla extends SimpleMagneticFluxDensityUnit("teratesla", "TT", r"1e12")
  final case object petatesla extends SimpleMagneticFluxDensityUnit("petatesla", "PT", r"1e15")
  final case object exatesla extends SimpleMagneticFluxDensityUnit("exatesla", "ET", r"1e18")
  final case object zettatesla extends SimpleMagneticFluxDensityUnit("zettatesla", "ZT", r"1e21")
  final case object yottatesla extends SimpleMagneticFluxDensityUnit("yottatesla", "YT", r"1e24")
  final case object gauss extends SimpleMagneticFluxDensityUnit("gauss", "G", r"1e-4")
  final case object yoctogauss extends SimpleMagneticFluxDensityUnit("yoctogauss", "yG", r"1e-4" * r"1e-24")
  final case object zeptogauss extends SimpleMagneticFluxDensityUnit("zeptogauss", "zG", r"1e-4" * r"1e-21")
  final case object attogauss extends SimpleMagneticFluxDensityUnit("attogauss", "aG", r"1e-4" * r"1e-18")
  final case object femtogauss extends SimpleMagneticFluxDensityUnit("femtogauss", "fG", r"1e-4" * r"1e-15")
  final case object picogauss extends SimpleMagneticFluxDensityUnit("picogauss", "pG", r"1e-4" * r"1e-12")
  final case object nanogauss extends SimpleMagneticFluxDensityUnit("nanogauss", "nG", r"1e-4" * r"1e-9")
  final case object microgauss extends DefaultMagneticFluxDensityUnit("microgauss", "μG", Seq("mcG"), r"1e-4" * r"1e-6")
  final case object milligauss extends SimpleMagneticFluxDensityUnit("milligauss", "mG", r"1e-4" * r"1e-3")
  final case object centigauss extends SimpleMagneticFluxDensityUnit("centigauss", "cG", r"1e-4" * r"1e-2")
  final case object decigauss extends SimpleMagneticFluxDensityUnit("decigauss", "dG", r"1e-4" * r"1e-1")
  final case object decagauss extends SimpleMagneticFluxDensityUnit("decagauss", "daG", r"1e-4" * r"1e1")
  final case object hectogauss extends SimpleMagneticFluxDensityUnit("hectogauss", "hG", r"1e-4" * r"1e2")
  final case object kilogauss extends DefaultMagneticFluxDensityUnit("kilogauss", "kG", Seq("KG"), r"1e-4" * r"1e3")
  final case object megagauss extends SimpleMagneticFluxDensityUnit("megagauss", "MG", r"1e-4" * r"1e6")
  final case object gigagauss extends SimpleMagneticFluxDensityUnit("gigagauss", "GG", r"1e-4" * r"1e9")
  final case object teragauss extends SimpleMagneticFluxDensityUnit("teragauss", "TG", r"1e-4" * r"1e12")
  final case object petagauss extends SimpleMagneticFluxDensityUnit("petagauss", "PG", r"1e-4" * r"1e15")
  final case object exagauss extends SimpleMagneticFluxDensityUnit("exagauss", "EG", r"1e-4" * r"1e18")
  final case object zettagauss extends SimpleMagneticFluxDensityUnit("zettagauss", "ZG", r"1e-4" * r"1e21")
  final case object yottagauss extends SimpleMagneticFluxDensityUnit("yottagauss", "YG", r"1e-4" * r"1e24")
}

object MagneticFluxDensityUnits{

  def T: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.tesla
  def yT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yoctotesla
  def zT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zeptotesla
  def aT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.attotesla
  def fT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.femtotesla
  def pT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.picotesla
  def nT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.nanotesla
  def `μT`: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microtesla
  def mcT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microtesla
  def mT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.millitesla
  def cT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.centitesla
  def dT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decitesla
  def daT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decatesla
  def hT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.hectotesla
  def kT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilotesla
  def KT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilotesla
  def MT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.megatesla
  def GT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gigatesla
  def TT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.teratesla
  def PT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.petatesla
  def ET: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.exatesla
  def ZT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zettatesla
  def YT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yottatesla
  def G: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gauss
  def yG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yoctogauss
  def zG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zeptogauss
  def aG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.attogauss
  def fG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.femtogauss
  def pG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.picogauss
  def nG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.nanogauss
  def `μG`: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microgauss
  def mcG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microgauss
  def mG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.milligauss
  def cG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.centigauss
  def dG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decigauss
  def daG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decagauss
  def hG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.hectogauss
  def kG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilogauss
  def KG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilogauss
  def MG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.megagauss
  def GG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gigagauss
  def TG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.teragauss
  def PG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.petagauss
  def EG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.exagauss
  def ZG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zettagauss
  def YG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yottagauss
}