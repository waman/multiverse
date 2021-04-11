package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.Constants

class MagneticFlux[A: Fractional](val value: A, val unit: MagneticFluxUnit)
    extends LinearQuantity[MagneticFlux[A], A, MagneticFluxUnit] {

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
    Seq(weber, yoctoweber, zeptoweber, attoweber, femtoweber, picoweber, nanoweber, microweber, milliweber, centiweber, deciweber, decaweber, hectoweber, kiloweber, megaweber, gigaweber, teraweber, petaweber, exaweber, zettaweber, yottaweber, maxwell, yoctomaxwell, zeptomaxwell, attomaxwell, femtomaxwell, picomaxwell, nanomaxwell, micromaxwell, millimaxwell, centimaxwell, decimaxwell, decamaxwell, hectomaxwell, kilomaxwell, megamaxwell, gigamaxwell, teramaxwell, petamaxwell, examaxwell, zettamaxwell, yottamaxwell, statweber)
}


/** For no aliase or user defined units */
class SimpleMagneticFluxUnit(val name: String, val symbol: String, val interval: Real) extends MagneticFluxUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultMagneticFluxUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MagneticFluxUnit
  
object MagneticFluxUnitObjects{

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
  final case object statweber extends SimpleMagneticFluxUnit("statweber", "statWb", Constants.SpeedOfLight * r"1e-6")
}


object MagneticFluxUnits{

  /** weber */
  def Wb: MagneticFluxUnit = MagneticFluxUnitObjects.weber
  /** yoctoweber */
  def yWb: MagneticFluxUnit = MagneticFluxUnitObjects.yoctoweber
  /** zeptoweber */
  def zWb: MagneticFluxUnit = MagneticFluxUnitObjects.zeptoweber
  /** attoweber */
  def aWb: MagneticFluxUnit = MagneticFluxUnitObjects.attoweber
  /** femtoweber */
  def fWb: MagneticFluxUnit = MagneticFluxUnitObjects.femtoweber
  /** picoweber */
  def pWb: MagneticFluxUnit = MagneticFluxUnitObjects.picoweber
  /** nanoweber */
  def nWb: MagneticFluxUnit = MagneticFluxUnitObjects.nanoweber
  /** microweber */
  def μWb: MagneticFluxUnit = MagneticFluxUnitObjects.microweber
  /** microweber */
  def mcWb: MagneticFluxUnit = MagneticFluxUnitObjects.microweber
  /** milliweber */
  def mWb: MagneticFluxUnit = MagneticFluxUnitObjects.milliweber
  /** centiweber */
  def cWb: MagneticFluxUnit = MagneticFluxUnitObjects.centiweber
  /** deciweber */
  def dWb: MagneticFluxUnit = MagneticFluxUnitObjects.deciweber
  /** decaweber */
  def daWb: MagneticFluxUnit = MagneticFluxUnitObjects.decaweber
  /** hectoweber */
  def hWb: MagneticFluxUnit = MagneticFluxUnitObjects.hectoweber
  /** kiloweber */
  def kWb: MagneticFluxUnit = MagneticFluxUnitObjects.kiloweber
  /** kiloweber */
  def KWb: MagneticFluxUnit = MagneticFluxUnitObjects.kiloweber
  /** megaweber */
  def MWb: MagneticFluxUnit = MagneticFluxUnitObjects.megaweber
  /** gigaweber */
  def GWb: MagneticFluxUnit = MagneticFluxUnitObjects.gigaweber
  /** teraweber */
  def TWb: MagneticFluxUnit = MagneticFluxUnitObjects.teraweber
  /** petaweber */
  def PWb: MagneticFluxUnit = MagneticFluxUnitObjects.petaweber
  /** exaweber */
  def EWb: MagneticFluxUnit = MagneticFluxUnitObjects.exaweber
  /** zettaweber */
  def ZWb: MagneticFluxUnit = MagneticFluxUnitObjects.zettaweber
  /** yottaweber */
  def YWb: MagneticFluxUnit = MagneticFluxUnitObjects.yottaweber
  /** maxwell */
  def Mx: MagneticFluxUnit = MagneticFluxUnitObjects.maxwell
  /** yoctomaxwell */
  def yMx: MagneticFluxUnit = MagneticFluxUnitObjects.yoctomaxwell
  /** zeptomaxwell */
  def zMx: MagneticFluxUnit = MagneticFluxUnitObjects.zeptomaxwell
  /** attomaxwell */
  def aMx: MagneticFluxUnit = MagneticFluxUnitObjects.attomaxwell
  /** femtomaxwell */
  def fMx: MagneticFluxUnit = MagneticFluxUnitObjects.femtomaxwell
  /** picomaxwell */
  def pMx: MagneticFluxUnit = MagneticFluxUnitObjects.picomaxwell
  /** nanomaxwell */
  def nMx: MagneticFluxUnit = MagneticFluxUnitObjects.nanomaxwell
  /** micromaxwell */
  def μMx: MagneticFluxUnit = MagneticFluxUnitObjects.micromaxwell
  /** micromaxwell */
  def mcMx: MagneticFluxUnit = MagneticFluxUnitObjects.micromaxwell
  /** millimaxwell */
  def mMx: MagneticFluxUnit = MagneticFluxUnitObjects.millimaxwell
  /** centimaxwell */
  def cMx: MagneticFluxUnit = MagneticFluxUnitObjects.centimaxwell
  /** decimaxwell */
  def dMx: MagneticFluxUnit = MagneticFluxUnitObjects.decimaxwell
  /** decamaxwell */
  def daMx: MagneticFluxUnit = MagneticFluxUnitObjects.decamaxwell
  /** hectomaxwell */
  def hMx: MagneticFluxUnit = MagneticFluxUnitObjects.hectomaxwell
  /** kilomaxwell */
  def kMx: MagneticFluxUnit = MagneticFluxUnitObjects.kilomaxwell
  /** kilomaxwell */
  def KMx: MagneticFluxUnit = MagneticFluxUnitObjects.kilomaxwell
  /** megamaxwell */
  def MMx: MagneticFluxUnit = MagneticFluxUnitObjects.megamaxwell
  /** gigamaxwell */
  def GMx: MagneticFluxUnit = MagneticFluxUnitObjects.gigamaxwell
  /** teramaxwell */
  def TMx: MagneticFluxUnit = MagneticFluxUnitObjects.teramaxwell
  /** petamaxwell */
  def PMx: MagneticFluxUnit = MagneticFluxUnitObjects.petamaxwell
  /** examaxwell */
  def EMx: MagneticFluxUnit = MagneticFluxUnitObjects.examaxwell
  /** zettamaxwell */
  def ZMx: MagneticFluxUnit = MagneticFluxUnitObjects.zettamaxwell
  /** yottamaxwell */
  def YMx: MagneticFluxUnit = MagneticFluxUnitObjects.yottamaxwell
  /** statweber */
  def statWb: MagneticFluxUnit = MagneticFluxUnitObjects.statweber
}