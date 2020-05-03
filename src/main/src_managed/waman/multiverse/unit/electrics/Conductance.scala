package waman.multiverse.unit.electrics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


class Conductance[A: Fractional](val value: A, val unit: ConductanceUnit)
    extends LinearQuantity[Conductance[A], A, ConductanceUnit] {

  import spire.implicits._


  def toResistance: Resistance[A] =
    new Resistance(apply(ConductanceUnitObjects.siemens).reciprocal, ResistanceUnitObjects.ohm)

  override protected def newQuantity(value: A, unit: ConductanceUnit): Conductance[A] = new Conductance(value, unit)
}

trait ConductanceUnit extends LinearUnit[ConductanceUnit]{

  override def getSIUnit: ConductanceUnit = ConductanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ConductanceUnit.dimension
}

object ConductanceUnit extends UnitInfo[ConductanceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 3, M -> -1, I -> 2, L -> -2).withDefaultValue(0)

  def getSIUnit: ConductanceUnit = ConductanceUnitObjects.siemens

  import ConductanceUnitObjects._
  def getUnits: Seq[ConductanceUnit] =
    Seq(siemens, yoctosiemens, zeptosiemens, attosiemens, femtosiemens, picosiemens, nanosiemens, microsiemens, millisiemens, centisiemens, decisiemens, decasiemens, hectosiemens, kilosiemens, megasiemens, gigasiemens, terasiemens, petasiemens, exasiemens, zettasiemens, yottasiemens)
}

/** For no aliase or user defined units */
class SimpleConductanceUnit(val name: String, val symbol: String, val interval: Real) extends ConductanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultConductanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ConductanceUnit

object ConductanceUnitObjects{

  import spire.implicits._


  final case object siemens extends DefaultConductanceUnit("siemens", "S", Seq("mho", "℧"), 1)
  final case object yoctosiemens extends DefaultConductanceUnit("yoctosiemens", "yS", Seq("ymho", "y℧"), r"1e-24")
  final case object zeptosiemens extends DefaultConductanceUnit("zeptosiemens", "zS", Seq("zmho", "z℧"), r"1e-21")
  final case object attosiemens extends DefaultConductanceUnit("attosiemens", "aS", Seq("amho", "a℧"), r"1e-18")
  final case object femtosiemens extends DefaultConductanceUnit("femtosiemens", "fS", Seq("fmho", "f℧"), r"1e-15")
  final case object picosiemens extends DefaultConductanceUnit("picosiemens", "pS", Seq("pmho", "p℧"), r"1e-12")
  final case object nanosiemens extends DefaultConductanceUnit("nanosiemens", "nS", Seq("nmho", "n℧"), r"1e-9")
  final case object microsiemens extends DefaultConductanceUnit("microsiemens", "μS", Seq("μmho", "μ℧", "mcS", "mcmho", "mc℧"), r"1e-6")
  final case object millisiemens extends DefaultConductanceUnit("millisiemens", "mS", Seq("mmho", "m℧"), r"1e-3")
  final case object centisiemens extends DefaultConductanceUnit("centisiemens", "cS", Seq("cmho", "c℧"), r"1e-2")
  final case object decisiemens extends DefaultConductanceUnit("decisiemens", "dS", Seq("dmho", "d℧"), r"1e-1")
  final case object decasiemens extends DefaultConductanceUnit("decasiemens", "daS", Seq("damho", "da℧"), r"1e1")
  final case object hectosiemens extends DefaultConductanceUnit("hectosiemens", "hS", Seq("hmho", "h℧"), r"1e2")
  final case object kilosiemens extends DefaultConductanceUnit("kilosiemens", "kS", Seq("kmho", "k℧", "KS", "Kmho", "K℧"), r"1e3")
  final case object megasiemens extends DefaultConductanceUnit("megasiemens", "MS", Seq("Mmho", "M℧"), r"1e6")
  final case object gigasiemens extends DefaultConductanceUnit("gigasiemens", "GS", Seq("Gmho", "G℧"), r"1e9")
  final case object terasiemens extends DefaultConductanceUnit("terasiemens", "TS", Seq("Tmho", "T℧"), r"1e12")
  final case object petasiemens extends DefaultConductanceUnit("petasiemens", "PS", Seq("Pmho", "P℧"), r"1e15")
  final case object exasiemens extends DefaultConductanceUnit("exasiemens", "ES", Seq("Emho", "E℧"), r"1e18")
  final case object zettasiemens extends DefaultConductanceUnit("zettasiemens", "ZS", Seq("Zmho", "Z℧"), r"1e21")
  final case object yottasiemens extends DefaultConductanceUnit("yottasiemens", "YS", Seq("Ymho", "Y℧"), r"1e24")
}

object ConductanceUnits{

  def S: ConductanceUnit = ConductanceUnitObjects.siemens
  def mho: ConductanceUnit = ConductanceUnitObjects.siemens
  def `℧`: ConductanceUnit = ConductanceUnitObjects.siemens
  def yS: ConductanceUnit = ConductanceUnitObjects.yoctosiemens
  def ymho: ConductanceUnit = ConductanceUnitObjects.yoctosiemens
  def `y℧`: ConductanceUnit = ConductanceUnitObjects.yoctosiemens
  def zS: ConductanceUnit = ConductanceUnitObjects.zeptosiemens
  def zmho: ConductanceUnit = ConductanceUnitObjects.zeptosiemens
  def `z℧`: ConductanceUnit = ConductanceUnitObjects.zeptosiemens
  def aS: ConductanceUnit = ConductanceUnitObjects.attosiemens
  def amho: ConductanceUnit = ConductanceUnitObjects.attosiemens
  def `a℧`: ConductanceUnit = ConductanceUnitObjects.attosiemens
  def fS: ConductanceUnit = ConductanceUnitObjects.femtosiemens
  def fmho: ConductanceUnit = ConductanceUnitObjects.femtosiemens
  def `f℧`: ConductanceUnit = ConductanceUnitObjects.femtosiemens
  def pS: ConductanceUnit = ConductanceUnitObjects.picosiemens
  def pmho: ConductanceUnit = ConductanceUnitObjects.picosiemens
  def `p℧`: ConductanceUnit = ConductanceUnitObjects.picosiemens
  def nS: ConductanceUnit = ConductanceUnitObjects.nanosiemens
  def nmho: ConductanceUnit = ConductanceUnitObjects.nanosiemens
  def `n℧`: ConductanceUnit = ConductanceUnitObjects.nanosiemens
  def `μS`: ConductanceUnit = ConductanceUnitObjects.microsiemens
  def `μmho`: ConductanceUnit = ConductanceUnitObjects.microsiemens
  def `μ℧`: ConductanceUnit = ConductanceUnitObjects.microsiemens
  def mcS: ConductanceUnit = ConductanceUnitObjects.microsiemens
  def mcmho: ConductanceUnit = ConductanceUnitObjects.microsiemens
  def `mc℧`: ConductanceUnit = ConductanceUnitObjects.microsiemens
  def mS: ConductanceUnit = ConductanceUnitObjects.millisiemens
  def mmho: ConductanceUnit = ConductanceUnitObjects.millisiemens
  def `m℧`: ConductanceUnit = ConductanceUnitObjects.millisiemens
  def cS: ConductanceUnit = ConductanceUnitObjects.centisiemens
  def cmho: ConductanceUnit = ConductanceUnitObjects.centisiemens
  def `c℧`: ConductanceUnit = ConductanceUnitObjects.centisiemens
  def dS: ConductanceUnit = ConductanceUnitObjects.decisiemens
  def dmho: ConductanceUnit = ConductanceUnitObjects.decisiemens
  def `d℧`: ConductanceUnit = ConductanceUnitObjects.decisiemens
  def daS: ConductanceUnit = ConductanceUnitObjects.decasiemens
  def damho: ConductanceUnit = ConductanceUnitObjects.decasiemens
  def `da℧`: ConductanceUnit = ConductanceUnitObjects.decasiemens
  def hS: ConductanceUnit = ConductanceUnitObjects.hectosiemens
  def hmho: ConductanceUnit = ConductanceUnitObjects.hectosiemens
  def `h℧`: ConductanceUnit = ConductanceUnitObjects.hectosiemens
  def kS: ConductanceUnit = ConductanceUnitObjects.kilosiemens
  def kmho: ConductanceUnit = ConductanceUnitObjects.kilosiemens
  def `k℧`: ConductanceUnit = ConductanceUnitObjects.kilosiemens
  def KS: ConductanceUnit = ConductanceUnitObjects.kilosiemens
  def Kmho: ConductanceUnit = ConductanceUnitObjects.kilosiemens
  def `K℧`: ConductanceUnit = ConductanceUnitObjects.kilosiemens
  def MS: ConductanceUnit = ConductanceUnitObjects.megasiemens
  def Mmho: ConductanceUnit = ConductanceUnitObjects.megasiemens
  def `M℧`: ConductanceUnit = ConductanceUnitObjects.megasiemens
  def GS: ConductanceUnit = ConductanceUnitObjects.gigasiemens
  def Gmho: ConductanceUnit = ConductanceUnitObjects.gigasiemens
  def `G℧`: ConductanceUnit = ConductanceUnitObjects.gigasiemens
  def TS: ConductanceUnit = ConductanceUnitObjects.terasiemens
  def Tmho: ConductanceUnit = ConductanceUnitObjects.terasiemens
  def `T℧`: ConductanceUnit = ConductanceUnitObjects.terasiemens
  def PS: ConductanceUnit = ConductanceUnitObjects.petasiemens
  def Pmho: ConductanceUnit = ConductanceUnitObjects.petasiemens
  def `P℧`: ConductanceUnit = ConductanceUnitObjects.petasiemens
  def ES: ConductanceUnit = ConductanceUnitObjects.exasiemens
  def Emho: ConductanceUnit = ConductanceUnitObjects.exasiemens
  def `E℧`: ConductanceUnit = ConductanceUnitObjects.exasiemens
  def ZS: ConductanceUnit = ConductanceUnitObjects.zettasiemens
  def Zmho: ConductanceUnit = ConductanceUnitObjects.zettasiemens
  def `Z℧`: ConductanceUnit = ConductanceUnitObjects.zettasiemens
  def YS: ConductanceUnit = ConductanceUnitObjects.yottasiemens
  def Ymho: ConductanceUnit = ConductanceUnitObjects.yottasiemens
  def `Y℧`: ConductanceUnit = ConductanceUnitObjects.yottasiemens
}