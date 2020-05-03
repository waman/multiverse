package waman.multiverse.unit.magnetics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


import waman.multiverse.unit.basic.Area
import waman.multiverse.unit.basic.AreaUnit


import waman.multiverse.unit.electrics.Current
import waman.multiverse.unit.electrics.CurrentUnit


class Flux[A: Fractional](val value: A, val unit: FluxUnit)
    extends LinearQuantity[Flux[A], A, FluxUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: FluxUnit): Flux[A] = new Flux(value, unit)

  def /(area: Area[A]): FluxDensity[A] = new FluxDensity(this.value / area.value, this.unit / area.unit)

  def /(current: Current[A]): Inductance[A] = new Inductance(this.value / current.value, this.unit / current.unit)
}

trait FluxUnit extends LinearUnit[FluxUnit]{

  override def getSIUnit: FluxUnit = FluxUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = FluxUnit.dimension

  def /(areaUnit: AreaUnit): FluxDensityUnit =
    new AbstractQuotientUnit[FluxDensityUnit, FluxUnit, AreaUnit](FluxUnit.this, areaUnit) with FluxDensityUnit

  def /(currentUnit: CurrentUnit): InductanceUnit =
    new AbstractQuotientUnit[InductanceUnit, FluxUnit, CurrentUnit](FluxUnit.this, currentUnit) with InductanceUnit
}

object FluxUnit extends UnitInfo[FluxUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, I -> -1, L -> 2).withDefaultValue(0)

  def getSIUnit: FluxUnit = FluxUnitObjects.weber

  import FluxUnitObjects._
  def getUnits: Seq[FluxUnit] =
    Seq(weber, yoctoweber, zeptoweber, attoweber, femtoweber, picoweber, nanoweber, microweber, milliweber, centiweber, deciweber, decaweber, hectoweber, kiloweber, megaweber, gigaweber, teraweber, petaweber, exaweber, zettaweber, yottaweber, maxwell, yoctomaxwell, zeptomaxwell, attomaxwell, femtomaxwell, picomaxwell, nanomaxwell, micromaxwell, millimaxwell, centimaxwell, decimaxwell, decamaxwell, hectomaxwell, kilomaxwell, megamaxwell, gigamaxwell, teramaxwell, petamaxwell, examaxwell, zettamaxwell, yottamaxwell)
}

/** For no aliase or user defined units */
class SimpleFluxUnit(val name: String, val symbol: String, val interval: Real) extends FluxUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultFluxUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends FluxUnit

object FluxUnitObjects{

  import spire.implicits._


  final case object weber extends SimpleFluxUnit("weber", "Wb", 1)
  final case object yoctoweber extends SimpleFluxUnit("yoctoweber", "yWb", r"1e-24")
  final case object zeptoweber extends SimpleFluxUnit("zeptoweber", "zWb", r"1e-21")
  final case object attoweber extends SimpleFluxUnit("attoweber", "aWb", r"1e-18")
  final case object femtoweber extends SimpleFluxUnit("femtoweber", "fWb", r"1e-15")
  final case object picoweber extends SimpleFluxUnit("picoweber", "pWb", r"1e-12")
  final case object nanoweber extends SimpleFluxUnit("nanoweber", "nWb", r"1e-9")
  final case object microweber extends DefaultFluxUnit("microweber", "μWb", Seq("mcWb"), r"1e-6")
  final case object milliweber extends SimpleFluxUnit("milliweber", "mWb", r"1e-3")
  final case object centiweber extends SimpleFluxUnit("centiweber", "cWb", r"1e-2")
  final case object deciweber extends SimpleFluxUnit("deciweber", "dWb", r"1e-1")
  final case object decaweber extends SimpleFluxUnit("decaweber", "daWb", r"1e1")
  final case object hectoweber extends SimpleFluxUnit("hectoweber", "hWb", r"1e2")
  final case object kiloweber extends DefaultFluxUnit("kiloweber", "kWb", Seq("KWb"), r"1e3")
  final case object megaweber extends SimpleFluxUnit("megaweber", "MWb", r"1e6")
  final case object gigaweber extends SimpleFluxUnit("gigaweber", "GWb", r"1e9")
  final case object teraweber extends SimpleFluxUnit("teraweber", "TWb", r"1e12")
  final case object petaweber extends SimpleFluxUnit("petaweber", "PWb", r"1e15")
  final case object exaweber extends SimpleFluxUnit("exaweber", "EWb", r"1e18")
  final case object zettaweber extends SimpleFluxUnit("zettaweber", "ZWb", r"1e21")
  final case object yottaweber extends SimpleFluxUnit("yottaweber", "YWb", r"1e24")
  final case object maxwell extends SimpleFluxUnit("maxwell", "Mx", r"1e-8")
  final case object yoctomaxwell extends SimpleFluxUnit("yoctomaxwell", "yMx", r"1e-8" * r"1e-24")
  final case object zeptomaxwell extends SimpleFluxUnit("zeptomaxwell", "zMx", r"1e-8" * r"1e-21")
  final case object attomaxwell extends SimpleFluxUnit("attomaxwell", "aMx", r"1e-8" * r"1e-18")
  final case object femtomaxwell extends SimpleFluxUnit("femtomaxwell", "fMx", r"1e-8" * r"1e-15")
  final case object picomaxwell extends SimpleFluxUnit("picomaxwell", "pMx", r"1e-8" * r"1e-12")
  final case object nanomaxwell extends SimpleFluxUnit("nanomaxwell", "nMx", r"1e-8" * r"1e-9")
  final case object micromaxwell extends DefaultFluxUnit("micromaxwell", "μMx", Seq("mcMx"), r"1e-8" * r"1e-6")
  final case object millimaxwell extends SimpleFluxUnit("millimaxwell", "mMx", r"1e-8" * r"1e-3")
  final case object centimaxwell extends SimpleFluxUnit("centimaxwell", "cMx", r"1e-8" * r"1e-2")
  final case object decimaxwell extends SimpleFluxUnit("decimaxwell", "dMx", r"1e-8" * r"1e-1")
  final case object decamaxwell extends SimpleFluxUnit("decamaxwell", "daMx", r"1e-8" * r"1e1")
  final case object hectomaxwell extends SimpleFluxUnit("hectomaxwell", "hMx", r"1e-8" * r"1e2")
  final case object kilomaxwell extends DefaultFluxUnit("kilomaxwell", "kMx", Seq("KMx"), r"1e-8" * r"1e3")
  final case object megamaxwell extends SimpleFluxUnit("megamaxwell", "MMx", r"1e-8" * r"1e6")
  final case object gigamaxwell extends SimpleFluxUnit("gigamaxwell", "GMx", r"1e-8" * r"1e9")
  final case object teramaxwell extends SimpleFluxUnit("teramaxwell", "TMx", r"1e-8" * r"1e12")
  final case object petamaxwell extends SimpleFluxUnit("petamaxwell", "PMx", r"1e-8" * r"1e15")
  final case object examaxwell extends SimpleFluxUnit("examaxwell", "EMx", r"1e-8" * r"1e18")
  final case object zettamaxwell extends SimpleFluxUnit("zettamaxwell", "ZMx", r"1e-8" * r"1e21")
  final case object yottamaxwell extends SimpleFluxUnit("yottamaxwell", "YMx", r"1e-8" * r"1e24")
}

object FluxUnits{

  def Wb: FluxUnit = FluxUnitObjects.weber
  def yWb: FluxUnit = FluxUnitObjects.yoctoweber
  def zWb: FluxUnit = FluxUnitObjects.zeptoweber
  def aWb: FluxUnit = FluxUnitObjects.attoweber
  def fWb: FluxUnit = FluxUnitObjects.femtoweber
  def pWb: FluxUnit = FluxUnitObjects.picoweber
  def nWb: FluxUnit = FluxUnitObjects.nanoweber
  def `μWb`: FluxUnit = FluxUnitObjects.microweber
  def mcWb: FluxUnit = FluxUnitObjects.microweber
  def mWb: FluxUnit = FluxUnitObjects.milliweber
  def cWb: FluxUnit = FluxUnitObjects.centiweber
  def dWb: FluxUnit = FluxUnitObjects.deciweber
  def daWb: FluxUnit = FluxUnitObjects.decaweber
  def hWb: FluxUnit = FluxUnitObjects.hectoweber
  def kWb: FluxUnit = FluxUnitObjects.kiloweber
  def KWb: FluxUnit = FluxUnitObjects.kiloweber
  def MWb: FluxUnit = FluxUnitObjects.megaweber
  def GWb: FluxUnit = FluxUnitObjects.gigaweber
  def TWb: FluxUnit = FluxUnitObjects.teraweber
  def PWb: FluxUnit = FluxUnitObjects.petaweber
  def EWb: FluxUnit = FluxUnitObjects.exaweber
  def ZWb: FluxUnit = FluxUnitObjects.zettaweber
  def YWb: FluxUnit = FluxUnitObjects.yottaweber
  def Mx: FluxUnit = FluxUnitObjects.maxwell
  def yMx: FluxUnit = FluxUnitObjects.yoctomaxwell
  def zMx: FluxUnit = FluxUnitObjects.zeptomaxwell
  def aMx: FluxUnit = FluxUnitObjects.attomaxwell
  def fMx: FluxUnit = FluxUnitObjects.femtomaxwell
  def pMx: FluxUnit = FluxUnitObjects.picomaxwell
  def nMx: FluxUnit = FluxUnitObjects.nanomaxwell
  def `μMx`: FluxUnit = FluxUnitObjects.micromaxwell
  def mcMx: FluxUnit = FluxUnitObjects.micromaxwell
  def mMx: FluxUnit = FluxUnitObjects.millimaxwell
  def cMx: FluxUnit = FluxUnitObjects.centimaxwell
  def dMx: FluxUnit = FluxUnitObjects.decimaxwell
  def daMx: FluxUnit = FluxUnitObjects.decamaxwell
  def hMx: FluxUnit = FluxUnitObjects.hectomaxwell
  def kMx: FluxUnit = FluxUnitObjects.kilomaxwell
  def KMx: FluxUnit = FluxUnitObjects.kilomaxwell
  def MMx: FluxUnit = FluxUnitObjects.megamaxwell
  def GMx: FluxUnit = FluxUnitObjects.gigamaxwell
  def TMx: FluxUnit = FluxUnitObjects.teramaxwell
  def PMx: FluxUnit = FluxUnitObjects.petamaxwell
  def EMx: FluxUnit = FluxUnitObjects.examaxwell
  def ZMx: FluxUnit = FluxUnitObjects.zettamaxwell
  def YMx: FluxUnit = FluxUnitObjects.yottamaxwell
}