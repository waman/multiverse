package org.waman.multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import org.waman.multiverse._


class ElectricalResistance[A: Fractional](val value: A, val unit: ElectricalResistanceUnit)
    extends LinearQuantity[ElectricalResistance[A], A, ElectricalResistanceUnit] {

  import spire.implicits._


  def toElectricalConductance: ElectricalConductance[A] =
    new ElectricalConductance(apply(ElectricalResistanceUnitObjects.ohm).reciprocal, ElectricalConductanceUnitObjects.siemens)

  override protected def newQuantity(value: A, unit: ElectricalResistanceUnit): ElectricalResistance[A] = new ElectricalResistance(value, unit)
}

trait ElectricalResistanceUnit extends LinearUnit[ElectricalResistanceUnit]{

  override def getSIUnit: ElectricalResistanceUnit = ElectricalResistanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ElectricalResistanceUnit.dimension
}

object ElectricalResistanceUnit extends UnitInfo[ElectricalResistanceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, I -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.ohm

  import ElectricalResistanceUnitObjects._
  def getUnits: Seq[ElectricalResistanceUnit] =
    Seq(ohm, yoctoohm, zeptoohm, attoohm, femtoohm, picoohm, nanoohm, microohm, milliohm, centiohm, deciohm, decaohm, hectoohm, kiloohm, megaohm, gigaohm, teraohm, petaohm, exaohm, zettaohm, yottaohm, abohm, statohm)
}

/** For no aliase or user defined units */
class SimpleElectricalResistanceUnit(val name: String, val symbol: String, val interval: Real) extends ElectricalResistanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultElectricalResistanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ElectricalResistanceUnit

object ElectricalResistanceUnitObjects{

  import spire.implicits._

  import org.waman.multiverse.unit.Constants

  final case object ohm extends DefaultElectricalResistanceUnit("ohm", "Ω", Seq("ohm"), 1)
  final case object yoctoohm extends DefaultElectricalResistanceUnit("yoctoohm", "yΩ", Seq("yohm"), r"1e-24")
  final case object zeptoohm extends DefaultElectricalResistanceUnit("zeptoohm", "zΩ", Seq("zohm"), r"1e-21")
  final case object attoohm extends DefaultElectricalResistanceUnit("attoohm", "aΩ", Seq("aohm"), r"1e-18")
  final case object femtoohm extends DefaultElectricalResistanceUnit("femtoohm", "fΩ", Seq("fohm"), r"1e-15")
  final case object picoohm extends DefaultElectricalResistanceUnit("picoohm", "pΩ", Seq("pohm"), r"1e-12")
  final case object nanoohm extends DefaultElectricalResistanceUnit("nanoohm", "nΩ", Seq("nohm"), r"1e-9")
  final case object microohm extends DefaultElectricalResistanceUnit("microohm", "μΩ", Seq("μohm", "mcΩ", "mcohm"), r"1e-6")
  final case object milliohm extends DefaultElectricalResistanceUnit("milliohm", "mΩ", Seq("mohm"), r"1e-3")
  final case object centiohm extends DefaultElectricalResistanceUnit("centiohm", "cΩ", Seq("cohm"), r"1e-2")
  final case object deciohm extends DefaultElectricalResistanceUnit("deciohm", "dΩ", Seq("dohm"), r"1e-1")
  final case object decaohm extends DefaultElectricalResistanceUnit("decaohm", "daΩ", Seq("daohm"), r"1e1")
  final case object hectoohm extends DefaultElectricalResistanceUnit("hectoohm", "hΩ", Seq("hohm"), r"1e2")
  final case object kiloohm extends DefaultElectricalResistanceUnit("kiloohm", "kΩ", Seq("kohm", "KΩ", "Kohm"), r"1e3")
  final case object megaohm extends DefaultElectricalResistanceUnit("megaohm", "MΩ", Seq("Mohm"), r"1e6")
  final case object gigaohm extends DefaultElectricalResistanceUnit("gigaohm", "GΩ", Seq("Gohm"), r"1e9")
  final case object teraohm extends DefaultElectricalResistanceUnit("teraohm", "TΩ", Seq("Tohm"), r"1e12")
  final case object petaohm extends DefaultElectricalResistanceUnit("petaohm", "PΩ", Seq("Pohm"), r"1e15")
  final case object exaohm extends DefaultElectricalResistanceUnit("exaohm", "EΩ", Seq("Eohm"), r"1e18")
  final case object zettaohm extends DefaultElectricalResistanceUnit("zettaohm", "ZΩ", Seq("Zohm"), r"1e21")
  final case object yottaohm extends DefaultElectricalResistanceUnit("yottaohm", "YΩ", Seq("Yohm"), r"1e24")
  final case object abohm extends DefaultElectricalResistanceUnit("abohm", "abΩ", Seq("abohm"), r"1e-9")
  final case object statohm extends DefaultElectricalResistanceUnit("statohm", "statΩ", Seq("statohm"), Constants.SpeedOfLight * Constants.SpeedOfLight * r"1e-5") with Description {
    def description: String = "Formal unit for Gaussian and ESU CGS unit system."
  }
}

object ElectricalResistanceUnits{

  def `Ω`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.ohm
  def ohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.ohm
  def `yΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.yoctoohm
  def yohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.yoctoohm
  def `zΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.zeptoohm
  def zohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.zeptoohm
  def `aΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.attoohm
  def aohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.attoohm
  def `fΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.femtoohm
  def fohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.femtoohm
  def `pΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.picoohm
  def pohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.picoohm
  def `nΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.nanoohm
  def nohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.nanoohm
  def `μΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.microohm
  def `μohm`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.microohm
  def `mcΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.microohm
  def mcohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.microohm
  def `mΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.milliohm
  def mohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.milliohm
  def `cΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.centiohm
  def cohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.centiohm
  def `dΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.deciohm
  def dohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.deciohm
  def `daΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.decaohm
  def daohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.decaohm
  def `hΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.hectoohm
  def hohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.hectoohm
  def `kΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.kiloohm
  def kohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.kiloohm
  def `KΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.kiloohm
  def Kohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.kiloohm
  def `MΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.megaohm
  def Mohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.megaohm
  def `GΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.gigaohm
  def Gohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.gigaohm
  def `TΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.teraohm
  def Tohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.teraohm
  def `PΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.petaohm
  def Pohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.petaohm
  def `EΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.exaohm
  def Eohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.exaohm
  def `ZΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.zettaohm
  def Zohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.zettaohm
  def `YΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.yottaohm
  def Yohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.yottaohm
  def `abΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.abohm
  def abohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.abohm
  def `statΩ`: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.statohm
  def statohm: ElectricalResistanceUnit = ElectricalResistanceUnitObjects.statohm
}