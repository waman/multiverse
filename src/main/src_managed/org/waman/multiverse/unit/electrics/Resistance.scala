package org.waman.multiverse.unit.electrics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Resistance[A: Fractional](val value: A, val unit: ResistanceUnit)
    extends LinearQuantity[Resistance[A], A, ResistanceUnit] {

  override protected def newQuantity(value: A, unit: ResistanceUnit): Resistance[A] = new Resistance(value, unit)

  def toConductance: Conductance[A] =
    new Conductance(apply(ResistanceUnitObjects.ohm).reciprocal, ConductanceUnitObjects.siemens)

}

/** null */
trait ResistanceUnit extends LinearUnit[ResistanceUnit]{

  override def getSIUnit: ResistanceUnit = ResistanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ResistanceUnit.dimension
}

object ResistanceUnit extends UnitInfo[ResistanceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, I -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: ResistanceUnit = ResistanceUnitObjects.ohm

  import ResistanceUnitObjects._
  def getUnits: Seq[ResistanceUnit] =
    Seq(ohm, yoctoohm, zeptoohm, attoohm, femtoohm, picoohm, nanoohm, microohm, milliohm, centiohm, deciohm, decaohm, hectoohm, kiloohm, megaohm, gigaohm, teraohm, petaohm, exaohm, zettaohm, yottaohm, abohm)
}

/** For no aliase or user defined units */
class SimpleResistanceUnit(val name: String, val symbol: String, val interval: Real) extends ResistanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultResistanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ResistanceUnit

object ResistanceUnitObjects{

  final case object ohm extends DefaultResistanceUnit("ohm", "Ω", Seq("ohm"), 1)
  final case object yoctoohm extends DefaultResistanceUnit("yoctoohm", "yΩ", Seq("yohm"), r"1e-24")
  final case object zeptoohm extends DefaultResistanceUnit("zeptoohm", "zΩ", Seq("zohm"), r"1e-21")
  final case object attoohm extends DefaultResistanceUnit("attoohm", "aΩ", Seq("aohm"), r"1e-18")
  final case object femtoohm extends DefaultResistanceUnit("femtoohm", "fΩ", Seq("fohm"), r"1e-15")
  final case object picoohm extends DefaultResistanceUnit("picoohm", "pΩ", Seq("pohm"), r"1e-12")
  final case object nanoohm extends DefaultResistanceUnit("nanoohm", "nΩ", Seq("nohm"), r"1e-9")
  final case object microohm extends DefaultResistanceUnit("microohm", "μΩ", Seq("μohm", "mcΩ", "mcohm"), r"1e-6")
  final case object milliohm extends DefaultResistanceUnit("milliohm", "mΩ", Seq("mohm"), r"1e-3")
  final case object centiohm extends DefaultResistanceUnit("centiohm", "cΩ", Seq("cohm"), r"1e-2")
  final case object deciohm extends DefaultResistanceUnit("deciohm", "dΩ", Seq("dohm"), r"1e-1")
  final case object decaohm extends DefaultResistanceUnit("decaohm", "daΩ", Seq("daohm"), r"1e1")
  final case object hectoohm extends DefaultResistanceUnit("hectoohm", "hΩ", Seq("hohm"), r"1e2")
  final case object kiloohm extends DefaultResistanceUnit("kiloohm", "kΩ", Seq("kohm", "KΩ", "Kohm"), r"1e3")
  final case object megaohm extends DefaultResistanceUnit("megaohm", "MΩ", Seq("Mohm"), r"1e6")
  final case object gigaohm extends DefaultResistanceUnit("gigaohm", "GΩ", Seq("Gohm"), r"1e9")
  final case object teraohm extends DefaultResistanceUnit("teraohm", "TΩ", Seq("Tohm"), r"1e12")
  final case object petaohm extends DefaultResistanceUnit("petaohm", "PΩ", Seq("Pohm"), r"1e15")
  final case object exaohm extends DefaultResistanceUnit("exaohm", "EΩ", Seq("Eohm"), r"1e18")
  final case object zettaohm extends DefaultResistanceUnit("zettaohm", "ZΩ", Seq("Zohm"), r"1e21")
  final case object yottaohm extends DefaultResistanceUnit("yottaohm", "YΩ", Seq("Yohm"), r"1e24")
  final case object abohm extends DefaultResistanceUnit("abohm", "abΩ", Seq("abohm"), r"1e-9")
}

object ResistanceUnits{

  def `Ω`: ResistanceUnit = ResistanceUnitObjects.ohm
  def ohm: ResistanceUnit = ResistanceUnitObjects.ohm
  def `yΩ`: ResistanceUnit = ResistanceUnitObjects.yoctoohm
  def yohm: ResistanceUnit = ResistanceUnitObjects.yoctoohm
  def `zΩ`: ResistanceUnit = ResistanceUnitObjects.zeptoohm
  def zohm: ResistanceUnit = ResistanceUnitObjects.zeptoohm
  def `aΩ`: ResistanceUnit = ResistanceUnitObjects.attoohm
  def aohm: ResistanceUnit = ResistanceUnitObjects.attoohm
  def `fΩ`: ResistanceUnit = ResistanceUnitObjects.femtoohm
  def fohm: ResistanceUnit = ResistanceUnitObjects.femtoohm
  def `pΩ`: ResistanceUnit = ResistanceUnitObjects.picoohm
  def pohm: ResistanceUnit = ResistanceUnitObjects.picoohm
  def `nΩ`: ResistanceUnit = ResistanceUnitObjects.nanoohm
  def nohm: ResistanceUnit = ResistanceUnitObjects.nanoohm
  def `μΩ`: ResistanceUnit = ResistanceUnitObjects.microohm
  def `μohm`: ResistanceUnit = ResistanceUnitObjects.microohm
  def `mcΩ`: ResistanceUnit = ResistanceUnitObjects.microohm
  def mcohm: ResistanceUnit = ResistanceUnitObjects.microohm
  def `mΩ`: ResistanceUnit = ResistanceUnitObjects.milliohm
  def mohm: ResistanceUnit = ResistanceUnitObjects.milliohm
  def `cΩ`: ResistanceUnit = ResistanceUnitObjects.centiohm
  def cohm: ResistanceUnit = ResistanceUnitObjects.centiohm
  def `dΩ`: ResistanceUnit = ResistanceUnitObjects.deciohm
  def dohm: ResistanceUnit = ResistanceUnitObjects.deciohm
  def `daΩ`: ResistanceUnit = ResistanceUnitObjects.decaohm
  def daohm: ResistanceUnit = ResistanceUnitObjects.decaohm
  def `hΩ`: ResistanceUnit = ResistanceUnitObjects.hectoohm
  def hohm: ResistanceUnit = ResistanceUnitObjects.hectoohm
  def `kΩ`: ResistanceUnit = ResistanceUnitObjects.kiloohm
  def kohm: ResistanceUnit = ResistanceUnitObjects.kiloohm
  def `KΩ`: ResistanceUnit = ResistanceUnitObjects.kiloohm
  def Kohm: ResistanceUnit = ResistanceUnitObjects.kiloohm
  def `MΩ`: ResistanceUnit = ResistanceUnitObjects.megaohm
  def Mohm: ResistanceUnit = ResistanceUnitObjects.megaohm
  def `GΩ`: ResistanceUnit = ResistanceUnitObjects.gigaohm
  def Gohm: ResistanceUnit = ResistanceUnitObjects.gigaohm
  def `TΩ`: ResistanceUnit = ResistanceUnitObjects.teraohm
  def Tohm: ResistanceUnit = ResistanceUnitObjects.teraohm
  def `PΩ`: ResistanceUnit = ResistanceUnitObjects.petaohm
  def Pohm: ResistanceUnit = ResistanceUnitObjects.petaohm
  def `EΩ`: ResistanceUnit = ResistanceUnitObjects.exaohm
  def Eohm: ResistanceUnit = ResistanceUnitObjects.exaohm
  def `ZΩ`: ResistanceUnit = ResistanceUnitObjects.zettaohm
  def Zohm: ResistanceUnit = ResistanceUnitObjects.zettaohm
  def `YΩ`: ResistanceUnit = ResistanceUnitObjects.yottaohm
  def Yohm: ResistanceUnit = ResistanceUnitObjects.yottaohm
  def `abΩ`: ResistanceUnit = ResistanceUnitObjects.abohm
  def abohm: ResistanceUnit = ResistanceUnitObjects.abohm
}