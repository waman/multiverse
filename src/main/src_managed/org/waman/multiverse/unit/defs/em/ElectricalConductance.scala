package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._

class ElectricalConductance[A: Fractional](val value: A, val unit: ElectricalConductanceUnit)
    extends LinearQuantity[ElectricalConductance[A], A, ElectricalConductanceUnit] {


  def toElectricalResistance: ElectricalResistance[A] =
    new ElectricalResistance(apply(ElectricalConductanceUnitObjects.siemens).reciprocal, ElectricalResistanceUnitObjects.ohm)

  override protected def newQuantity(value: A, unit: ElectricalConductanceUnit): ElectricalConductance[A] = new ElectricalConductance(value, unit)
}

/** None */
trait ElectricalConductanceUnit extends LinearUnit[ElectricalConductanceUnit]{

  override def getSIUnit: ElectricalConductanceUnit = ElectricalConductanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ElectricalConductanceUnit.dimension
}

object ElectricalConductanceUnit extends UnitInfo[ElectricalConductanceUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 3, M -> -1, I -> 2, L -> -2).withDefaultValue(0)

  def getSIUnit: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.siemens

  import ElectricalConductanceUnitObjects._

  def getUnits: Seq[ElectricalConductanceUnit] =
    Seq(siemens, yoctosiemens, zeptosiemens, attosiemens, femtosiemens, picosiemens, nanosiemens, microsiemens, millisiemens, centisiemens, decisiemens, decasiemens, hectosiemens, kilosiemens, megasiemens, gigasiemens, terasiemens, petasiemens, exasiemens, zettasiemens, yottasiemens)
}


/** For no aliase or user defined units */
class SimpleElectricalConductanceUnit(val name: String, val symbol: String, val interval: Real) extends ElectricalConductanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultElectricalConductanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ElectricalConductanceUnit
  
object ElectricalConductanceUnitObjects{

  final case object siemens extends DefaultElectricalConductanceUnit("siemens", "S", Seq("mho", "℧"), 1)
  final case object yoctosiemens extends DefaultElectricalConductanceUnit("yoctosiemens", "yS", Seq("ymho", "y℧"), r"1e-24")
  final case object zeptosiemens extends DefaultElectricalConductanceUnit("zeptosiemens", "zS", Seq("zmho", "z℧"), r"1e-21")
  final case object attosiemens extends DefaultElectricalConductanceUnit("attosiemens", "aS", Seq("amho", "a℧"), r"1e-18")
  final case object femtosiemens extends DefaultElectricalConductanceUnit("femtosiemens", "fS", Seq("fmho", "f℧"), r"1e-15")
  final case object picosiemens extends DefaultElectricalConductanceUnit("picosiemens", "pS", Seq("pmho", "p℧"), r"1e-12")
  final case object nanosiemens extends DefaultElectricalConductanceUnit("nanosiemens", "nS", Seq("nmho", "n℧"), r"1e-9")
  final case object microsiemens extends DefaultElectricalConductanceUnit("microsiemens", "μS", Seq("mcS", "μmho", "mcmho", "μ℧", "mc℧"), r"1e-6")
  final case object millisiemens extends DefaultElectricalConductanceUnit("millisiemens", "mS", Seq("mmho", "m℧"), r"1e-3")
  final case object centisiemens extends DefaultElectricalConductanceUnit("centisiemens", "cS", Seq("cmho", "c℧"), r"1e-2")
  final case object decisiemens extends DefaultElectricalConductanceUnit("decisiemens", "dS", Seq("dmho", "d℧"), r"1e-1")
  final case object decasiemens extends DefaultElectricalConductanceUnit("decasiemens", "daS", Seq("damho", "da℧"), r"1e1")
  final case object hectosiemens extends DefaultElectricalConductanceUnit("hectosiemens", "hS", Seq("hmho", "h℧"), r"1e2")
  final case object kilosiemens extends DefaultElectricalConductanceUnit("kilosiemens", "kS", Seq("KS", "kmho", "Kmho", "k℧", "K℧"), r"1e3")
  final case object megasiemens extends DefaultElectricalConductanceUnit("megasiemens", "MS", Seq("Mmho", "M℧"), r"1e6")
  final case object gigasiemens extends DefaultElectricalConductanceUnit("gigasiemens", "GS", Seq("Gmho", "G℧"), r"1e9")
  final case object terasiemens extends DefaultElectricalConductanceUnit("terasiemens", "TS", Seq("Tmho", "T℧"), r"1e12")
  final case object petasiemens extends DefaultElectricalConductanceUnit("petasiemens", "PS", Seq("Pmho", "P℧"), r"1e15")
  final case object exasiemens extends DefaultElectricalConductanceUnit("exasiemens", "ES", Seq("Emho", "E℧"), r"1e18")
  final case object zettasiemens extends DefaultElectricalConductanceUnit("zettasiemens", "ZS", Seq("Zmho", "Z℧"), r"1e21")
  final case object yottasiemens extends DefaultElectricalConductanceUnit("yottasiemens", "YS", Seq("Ymho", "Y℧"), r"1e24")
}


object ElectricalConductanceUnits{

  /** siemens */
  def S: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.siemens
  /** siemens */
  def mho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.siemens
  /** siemens */
  def `℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.siemens
  /** yoctosiemens */
  def yS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.yoctosiemens
  /** yoctosiemens */
  def ymho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.yoctosiemens
  /** yoctosiemens */
  def `y℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.yoctosiemens
  /** zeptosiemens */
  def zS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.zeptosiemens
  /** zeptosiemens */
  def zmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.zeptosiemens
  /** zeptosiemens */
  def `z℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.zeptosiemens
  /** attosiemens */
  def aS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.attosiemens
  /** attosiemens */
  def amho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.attosiemens
  /** attosiemens */
  def `a℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.attosiemens
  /** femtosiemens */
  def fS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.femtosiemens
  /** femtosiemens */
  def fmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.femtosiemens
  /** femtosiemens */
  def `f℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.femtosiemens
  /** picosiemens */
  def pS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.picosiemens
  /** picosiemens */
  def pmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.picosiemens
  /** picosiemens */
  def `p℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.picosiemens
  /** nanosiemens */
  def nS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.nanosiemens
  /** nanosiemens */
  def nmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.nanosiemens
  /** nanosiemens */
  def `n℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.nanosiemens
  /** microsiemens */
  def μS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.microsiemens
  /** microsiemens */
  def mcS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.microsiemens
  /** microsiemens */
  def μmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.microsiemens
  /** microsiemens */
  def mcmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.microsiemens
  /** microsiemens */
  def `μ℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.microsiemens
  /** microsiemens */
  def `mc℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.microsiemens
  /** millisiemens */
  def mS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.millisiemens
  /** millisiemens */
  def mmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.millisiemens
  /** millisiemens */
  def `m℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.millisiemens
  /** centisiemens */
  def cS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.centisiemens
  /** centisiemens */
  def cmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.centisiemens
  /** centisiemens */
  def `c℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.centisiemens
  /** decisiemens */
  def dS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.decisiemens
  /** decisiemens */
  def dmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.decisiemens
  /** decisiemens */
  def `d℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.decisiemens
  /** decasiemens */
  def daS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.decasiemens
  /** decasiemens */
  def damho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.decasiemens
  /** decasiemens */
  def `da℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.decasiemens
  /** hectosiemens */
  def hS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.hectosiemens
  /** hectosiemens */
  def hmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.hectosiemens
  /** hectosiemens */
  def `h℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.hectosiemens
  /** kilosiemens */
  def kS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.kilosiemens
  /** kilosiemens */
  def KS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.kilosiemens
  /** kilosiemens */
  def kmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.kilosiemens
  /** kilosiemens */
  def Kmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.kilosiemens
  /** kilosiemens */
  def `k℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.kilosiemens
  /** kilosiemens */
  def `K℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.kilosiemens
  /** megasiemens */
  def MS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.megasiemens
  /** megasiemens */
  def Mmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.megasiemens
  /** megasiemens */
  def `M℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.megasiemens
  /** gigasiemens */
  def GS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.gigasiemens
  /** gigasiemens */
  def Gmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.gigasiemens
  /** gigasiemens */
  def `G℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.gigasiemens
  /** terasiemens */
  def TS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.terasiemens
  /** terasiemens */
  def Tmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.terasiemens
  /** terasiemens */
  def `T℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.terasiemens
  /** petasiemens */
  def PS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.petasiemens
  /** petasiemens */
  def Pmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.petasiemens
  /** petasiemens */
  def `P℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.petasiemens
  /** exasiemens */
  def ES: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.exasiemens
  /** exasiemens */
  def Emho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.exasiemens
  /** exasiemens */
  def `E℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.exasiemens
  /** zettasiemens */
  def ZS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.zettasiemens
  /** zettasiemens */
  def Zmho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.zettasiemens
  /** zettasiemens */
  def `Z℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.zettasiemens
  /** yottasiemens */
  def YS: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.yottasiemens
  /** yottasiemens */
  def Ymho: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.yottasiemens
  /** yottasiemens */
  def `Y℧`: ElectricalConductanceUnit = ElectricalConductanceUnitObjects.yottasiemens
}