package waman.multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


import waman.multiverse.unit.basic.Area
import waman.multiverse.unit.basic.AreaUnit


class MagneticFlux[A: Fractional](val value: A, val unit: MagneticFluxUnit)
    extends LinearQuantity[MagneticFlux[A], A, MagneticFluxUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: MagneticFluxUnit): MagneticFlux[A] = new MagneticFlux(value, unit)

  def /(electricCurrent: ElectricCurrent[A]): Inductance[A] = new Inductance(this.value / electricCurrent.value, this.unit / electricCurrent.unit)

  def /(area: Area[A]): MagneticFluxDensity[A] = new MagneticFluxDensity(this.value / area.value, this.unit / area.unit)
}

trait MagneticFluxUnit extends LinearUnit[MagneticFluxUnit]{

  override def getSIUnit: MagneticFluxUnit = MagneticFluxUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MagneticFluxUnit.dimension

  def /(electricCurrentUnit: ElectricCurrentUnit): InductanceUnit =
    new QuotientUnit[InductanceUnit, MagneticFluxUnit, ElectricCurrentUnit](MagneticFluxUnit.this, electricCurrentUnit) with InductanceUnit

  def /(areaUnit: AreaUnit): MagneticFluxDensityUnit =
    new QuotientUnit[MagneticFluxDensityUnit, MagneticFluxUnit, AreaUnit](MagneticFluxUnit.this, areaUnit) with MagneticFluxDensityUnit
}

object MagneticFluxUnit extends UnitInfo[MagneticFluxUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, I -> -1, L -> 2).withDefaultValue(0)

  def getSIUnit: MagneticFluxUnit = MagneticFluxUnitObjects.weber

  import MagneticFluxUnitObjects._
  def getUnits: Seq[MagneticFluxUnit] =
    Seq(weber, yoctoweber, zeptoweber, attoweber, femtoweber, picoweber, nanoweber, microweber, milliweber, centiweber, deciweber, decaweber, hectoweber, kiloweber, megaweber, gigaweber, teraweber, petaweber, exaweber, zettaweber, yottaweber, maxwell, yoctomaxwell, zeptomaxwell, attomaxwell, femtomaxwell, picomaxwell, nanomaxwell, micromaxwell, millimaxwell, centimaxwell, decimaxwell, decamaxwell, hectomaxwell, kilomaxwell, megamaxwell, gigamaxwell, teramaxwell, petamaxwell, examaxwell, zettamaxwell, yottamaxwell)
}

/** For no aliase or user defined units */
class SimpleMagneticFluxUnit(val name: String, val symbol: String, val interval: Real) extends MagneticFluxUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultMagneticFluxUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MagneticFluxUnit

object MagneticFluxUnitObjects{

  import spire.implicits._


  final case object weber extends SimpleMagneticFluxUnit("weber", "Wb", 1)
  final case object yoctoweber extends SimpleMagneticFluxUnit("yoctoweber", "yWb", r"1e-24")
  final case object zeptoweber extends SimpleMagneticFluxUnit("zeptoweber", "zWb", r"1e-21")
  final case object attoweber extends SimpleMagneticFluxUnit("attoweber", "aWb", r"1e-18")
  final case object femtoweber extends SimpleMagneticFluxUnit("femtoweber", "fWb", r"1e-15")
  final case object picoweber extends SimpleMagneticFluxUnit("picoweber", "pWb", r"1e-12")
  final case object nanoweber extends SimpleMagneticFluxUnit("nanoweber", "nWb", r"1e-9")
  final case object microweber extends DefaultMagneticFluxUnit("microweber", "μWb", Seq("mcWb"), r"1e-6")
  final case object milliweber extends SimpleMagneticFluxUnit("milliweber", "mWb", r"1e-3")
  final case object centiweber extends SimpleMagneticFluxUnit("centiweber", "cWb", r"1e-2")
  final case object deciweber extends SimpleMagneticFluxUnit("deciweber", "dWb", r"1e-1")
  final case object decaweber extends SimpleMagneticFluxUnit("decaweber", "daWb", r"1e1")
  final case object hectoweber extends SimpleMagneticFluxUnit("hectoweber", "hWb", r"1e2")
  final case object kiloweber extends DefaultMagneticFluxUnit("kiloweber", "kWb", Seq("KWb"), r"1e3")
  final case object megaweber extends SimpleMagneticFluxUnit("megaweber", "MWb", r"1e6")
  final case object gigaweber extends SimpleMagneticFluxUnit("gigaweber", "GWb", r"1e9")
  final case object teraweber extends SimpleMagneticFluxUnit("teraweber", "TWb", r"1e12")
  final case object petaweber extends SimpleMagneticFluxUnit("petaweber", "PWb", r"1e15")
  final case object exaweber extends SimpleMagneticFluxUnit("exaweber", "EWb", r"1e18")
  final case object zettaweber extends SimpleMagneticFluxUnit("zettaweber", "ZWb", r"1e21")
  final case object yottaweber extends SimpleMagneticFluxUnit("yottaweber", "YWb", r"1e24")
  final case object maxwell extends SimpleMagneticFluxUnit("maxwell", "Mx", r"1e-8")
  final case object yoctomaxwell extends SimpleMagneticFluxUnit("yoctomaxwell", "yMx", r"1e-8" * r"1e-24")
  final case object zeptomaxwell extends SimpleMagneticFluxUnit("zeptomaxwell", "zMx", r"1e-8" * r"1e-21")
  final case object attomaxwell extends SimpleMagneticFluxUnit("attomaxwell", "aMx", r"1e-8" * r"1e-18")
  final case object femtomaxwell extends SimpleMagneticFluxUnit("femtomaxwell", "fMx", r"1e-8" * r"1e-15")
  final case object picomaxwell extends SimpleMagneticFluxUnit("picomaxwell", "pMx", r"1e-8" * r"1e-12")
  final case object nanomaxwell extends SimpleMagneticFluxUnit("nanomaxwell", "nMx", r"1e-8" * r"1e-9")
  final case object micromaxwell extends DefaultMagneticFluxUnit("micromaxwell", "μMx", Seq("mcMx"), r"1e-8" * r"1e-6")
  final case object millimaxwell extends SimpleMagneticFluxUnit("millimaxwell", "mMx", r"1e-8" * r"1e-3")
  final case object centimaxwell extends SimpleMagneticFluxUnit("centimaxwell", "cMx", r"1e-8" * r"1e-2")
  final case object decimaxwell extends SimpleMagneticFluxUnit("decimaxwell", "dMx", r"1e-8" * r"1e-1")
  final case object decamaxwell extends SimpleMagneticFluxUnit("decamaxwell", "daMx", r"1e-8" * r"1e1")
  final case object hectomaxwell extends SimpleMagneticFluxUnit("hectomaxwell", "hMx", r"1e-8" * r"1e2")
  final case object kilomaxwell extends DefaultMagneticFluxUnit("kilomaxwell", "kMx", Seq("KMx"), r"1e-8" * r"1e3")
  final case object megamaxwell extends SimpleMagneticFluxUnit("megamaxwell", "MMx", r"1e-8" * r"1e6")
  final case object gigamaxwell extends SimpleMagneticFluxUnit("gigamaxwell", "GMx", r"1e-8" * r"1e9")
  final case object teramaxwell extends SimpleMagneticFluxUnit("teramaxwell", "TMx", r"1e-8" * r"1e12")
  final case object petamaxwell extends SimpleMagneticFluxUnit("petamaxwell", "PMx", r"1e-8" * r"1e15")
  final case object examaxwell extends SimpleMagneticFluxUnit("examaxwell", "EMx", r"1e-8" * r"1e18")
  final case object zettamaxwell extends SimpleMagneticFluxUnit("zettamaxwell", "ZMx", r"1e-8" * r"1e21")
  final case object yottamaxwell extends SimpleMagneticFluxUnit("yottamaxwell", "YMx", r"1e-8" * r"1e24")
}

object MagneticFluxUnits{

  def Wb: MagneticFluxUnit = MagneticFluxUnitObjects.weber
  def yWb: MagneticFluxUnit = MagneticFluxUnitObjects.yoctoweber
  def zWb: MagneticFluxUnit = MagneticFluxUnitObjects.zeptoweber
  def aWb: MagneticFluxUnit = MagneticFluxUnitObjects.attoweber
  def fWb: MagneticFluxUnit = MagneticFluxUnitObjects.femtoweber
  def pWb: MagneticFluxUnit = MagneticFluxUnitObjects.picoweber
  def nWb: MagneticFluxUnit = MagneticFluxUnitObjects.nanoweber
  def `μWb`: MagneticFluxUnit = MagneticFluxUnitObjects.microweber
  def mcWb: MagneticFluxUnit = MagneticFluxUnitObjects.microweber
  def mWb: MagneticFluxUnit = MagneticFluxUnitObjects.milliweber
  def cWb: MagneticFluxUnit = MagneticFluxUnitObjects.centiweber
  def dWb: MagneticFluxUnit = MagneticFluxUnitObjects.deciweber
  def daWb: MagneticFluxUnit = MagneticFluxUnitObjects.decaweber
  def hWb: MagneticFluxUnit = MagneticFluxUnitObjects.hectoweber
  def kWb: MagneticFluxUnit = MagneticFluxUnitObjects.kiloweber
  def KWb: MagneticFluxUnit = MagneticFluxUnitObjects.kiloweber
  def MWb: MagneticFluxUnit = MagneticFluxUnitObjects.megaweber
  def GWb: MagneticFluxUnit = MagneticFluxUnitObjects.gigaweber
  def TWb: MagneticFluxUnit = MagneticFluxUnitObjects.teraweber
  def PWb: MagneticFluxUnit = MagneticFluxUnitObjects.petaweber
  def EWb: MagneticFluxUnit = MagneticFluxUnitObjects.exaweber
  def ZWb: MagneticFluxUnit = MagneticFluxUnitObjects.zettaweber
  def YWb: MagneticFluxUnit = MagneticFluxUnitObjects.yottaweber
  def Mx: MagneticFluxUnit = MagneticFluxUnitObjects.maxwell
  def yMx: MagneticFluxUnit = MagneticFluxUnitObjects.yoctomaxwell
  def zMx: MagneticFluxUnit = MagneticFluxUnitObjects.zeptomaxwell
  def aMx: MagneticFluxUnit = MagneticFluxUnitObjects.attomaxwell
  def fMx: MagneticFluxUnit = MagneticFluxUnitObjects.femtomaxwell
  def pMx: MagneticFluxUnit = MagneticFluxUnitObjects.picomaxwell
  def nMx: MagneticFluxUnit = MagneticFluxUnitObjects.nanomaxwell
  def `μMx`: MagneticFluxUnit = MagneticFluxUnitObjects.micromaxwell
  def mcMx: MagneticFluxUnit = MagneticFluxUnitObjects.micromaxwell
  def mMx: MagneticFluxUnit = MagneticFluxUnitObjects.millimaxwell
  def cMx: MagneticFluxUnit = MagneticFluxUnitObjects.centimaxwell
  def dMx: MagneticFluxUnit = MagneticFluxUnitObjects.decimaxwell
  def daMx: MagneticFluxUnit = MagneticFluxUnitObjects.decamaxwell
  def hMx: MagneticFluxUnit = MagneticFluxUnitObjects.hectomaxwell
  def kMx: MagneticFluxUnit = MagneticFluxUnitObjects.kilomaxwell
  def KMx: MagneticFluxUnit = MagneticFluxUnitObjects.kilomaxwell
  def MMx: MagneticFluxUnit = MagneticFluxUnitObjects.megamaxwell
  def GMx: MagneticFluxUnit = MagneticFluxUnitObjects.gigamaxwell
  def TMx: MagneticFluxUnit = MagneticFluxUnitObjects.teramaxwell
  def PMx: MagneticFluxUnit = MagneticFluxUnitObjects.petamaxwell
  def EMx: MagneticFluxUnit = MagneticFluxUnitObjects.examaxwell
  def ZMx: MagneticFluxUnit = MagneticFluxUnitObjects.zettamaxwell
  def YMx: MagneticFluxUnit = MagneticFluxUnitObjects.yottamaxwell
}