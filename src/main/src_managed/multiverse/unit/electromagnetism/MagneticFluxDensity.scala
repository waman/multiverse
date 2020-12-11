package multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import multiverse._


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
    Seq(tesla, yoctotesla, zeptotesla, attotesla, femtotesla, picotesla, nanotesla, microtesla, millitesla, centitesla, decitesla, decatesla, hectotesla, kilotesla, megatesla, gigatesla, teratesla, petatesla, exatesla, zettatesla, yottatesla, gauss, yoctogauss, zeptogauss, attogauss, femtogauss, picogauss, nanogauss, microgauss, milligauss, centigauss, decigauss, decagauss, hectogauss, kilogauss, megagauss, gigagauss, teragauss, petagauss, exagauss, zettagauss, yottagauss, stattesla)
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

  import multiverse.unit.Constants

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
  final case object gauss extends DefaultMagneticFluxDensityUnit("gauss", "G", Seq("Gs"), r"1e-4")
  final case object yoctogauss extends DefaultMagneticFluxDensityUnit("yoctogauss", "yG", Seq("yGs"), r"1e-4" * r"1e-24")
  final case object zeptogauss extends DefaultMagneticFluxDensityUnit("zeptogauss", "zG", Seq("zGs"), r"1e-4" * r"1e-21")
  final case object attogauss extends DefaultMagneticFluxDensityUnit("attogauss", "aG", Seq("aGs"), r"1e-4" * r"1e-18")
  final case object femtogauss extends DefaultMagneticFluxDensityUnit("femtogauss", "fG", Seq("fGs"), r"1e-4" * r"1e-15")
  final case object picogauss extends DefaultMagneticFluxDensityUnit("picogauss", "pG", Seq("pGs"), r"1e-4" * r"1e-12")
  final case object nanogauss extends DefaultMagneticFluxDensityUnit("nanogauss", "nG", Seq("nGs"), r"1e-4" * r"1e-9")
  final case object microgauss extends DefaultMagneticFluxDensityUnit("microgauss", "μG", Seq("μGs", "mcG", "mcGs"), r"1e-4" * r"1e-6")
  final case object milligauss extends DefaultMagneticFluxDensityUnit("milligauss", "mG", Seq("mGs"), r"1e-4" * r"1e-3")
  final case object centigauss extends DefaultMagneticFluxDensityUnit("centigauss", "cG", Seq("cGs"), r"1e-4" * r"1e-2")
  final case object decigauss extends DefaultMagneticFluxDensityUnit("decigauss", "dG", Seq("dGs"), r"1e-4" * r"1e-1")
  final case object decagauss extends DefaultMagneticFluxDensityUnit("decagauss", "daG", Seq("daGs"), r"1e-4" * r"1e1")
  final case object hectogauss extends DefaultMagneticFluxDensityUnit("hectogauss", "hG", Seq("hGs"), r"1e-4" * r"1e2")
  final case object kilogauss extends DefaultMagneticFluxDensityUnit("kilogauss", "kG", Seq("kGs", "KG", "KGs"), r"1e-4" * r"1e3")
  final case object megagauss extends DefaultMagneticFluxDensityUnit("megagauss", "MG", Seq("MGs"), r"1e-4" * r"1e6")
  final case object gigagauss extends DefaultMagneticFluxDensityUnit("gigagauss", "GG", Seq("GGs"), r"1e-4" * r"1e9")
  final case object teragauss extends DefaultMagneticFluxDensityUnit("teragauss", "TG", Seq("TGs"), r"1e-4" * r"1e12")
  final case object petagauss extends DefaultMagneticFluxDensityUnit("petagauss", "PG", Seq("PGs"), r"1e-4" * r"1e15")
  final case object exagauss extends DefaultMagneticFluxDensityUnit("exagauss", "EG", Seq("EGs"), r"1e-4" * r"1e18")
  final case object zettagauss extends DefaultMagneticFluxDensityUnit("zettagauss", "ZG", Seq("ZGs"), r"1e-4" * r"1e21")
  final case object yottagauss extends DefaultMagneticFluxDensityUnit("yottagauss", "YG", Seq("YGs"), r"1e-4" * r"1e24")
  final case object stattesla extends SimpleMagneticFluxDensityUnit("stattesla", "statT", Constants.SpeedOfLight * r"1e-2")
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
  def Gs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gauss
  def yG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yoctogauss
  def yGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yoctogauss
  def zG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zeptogauss
  def zGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zeptogauss
  def aG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.attogauss
  def aGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.attogauss
  def fG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.femtogauss
  def fGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.femtogauss
  def pG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.picogauss
  def pGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.picogauss
  def nG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.nanogauss
  def nGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.nanogauss
  def `μG`: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microgauss
  def `μGs`: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microgauss
  def mcG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microgauss
  def mcGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.microgauss
  def mG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.milligauss
  def mGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.milligauss
  def cG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.centigauss
  def cGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.centigauss
  def dG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decigauss
  def dGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decigauss
  def daG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decagauss
  def daGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.decagauss
  def hG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.hectogauss
  def hGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.hectogauss
  def kG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilogauss
  def kGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilogauss
  def KG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilogauss
  def KGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.kilogauss
  def MG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.megagauss
  def MGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.megagauss
  def GG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gigagauss
  def GGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.gigagauss
  def TG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.teragauss
  def TGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.teragauss
  def PG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.petagauss
  def PGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.petagauss
  def EG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.exagauss
  def EGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.exagauss
  def ZG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zettagauss
  def ZGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.zettagauss
  def YG: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yottagauss
  def YGs: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.yottagauss
  def statT: MagneticFluxDensityUnit = MagneticFluxDensityUnitObjects.stattesla
}