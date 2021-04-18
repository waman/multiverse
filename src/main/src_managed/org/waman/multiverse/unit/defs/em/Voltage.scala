package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.Constants

class Voltage[A: Fractional](val value: A, val unit: VoltageUnit)
    extends LinearQuantity[Voltage[A], A, VoltageUnit] {

  override protected def newQuantity(value: A, unit: VoltageUnit): Voltage[A] = new Voltage(value, unit)

  def /(electricCurrent: ElectricCurrent[A]): ElectricalResistance[A] = new ElectricalResistance(this.value / electricCurrent.value, this.unit / electricCurrent.unit)

  def *(time: Time[A]): MagneticFlux[A] = new MagneticFlux(this.value * time.value, this.unit * time.unit)
}

/** None */
trait VoltageUnit extends LinearUnit[VoltageUnit]{

  override def getSIUnit: VoltageUnit = VoltageUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VoltageUnit.dimension

  def /(electricCurrentUnit: ElectricCurrentUnit): ElectricalResistanceUnit =
    new QuotientUnit[ElectricalResistanceUnit, VoltageUnit, ElectricCurrentUnit](VoltageUnit.this, electricCurrentUnit) with ElectricalResistanceUnit

  def *(timeUnit: TimeUnit): MagneticFluxUnit =
    new ProductUnit[MagneticFluxUnit, VoltageUnit, TimeUnit](VoltageUnit.this, timeUnit) with MagneticFluxUnit
}

object VoltageUnit extends UnitInfo[VoltageUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, I -> -1, L -> 2).withDefaultValue(0)

  def getSIUnit: VoltageUnit = VoltageUnitObjects.volt

  import VoltageUnitObjects._

  def getUnits: Seq[VoltageUnit] =
    Seq(volt, yoctovolt, zeptovolt, attovolt, femtovolt, picovolt, nanovolt, microvolt, millivolt, centivolt, decivolt, decavolt, hectovolt, kilovolt, megavolt, gigavolt, teravolt, petavolt, exavolt, zettavolt, yottavolt, abvolt, statvolt)
}


/** For no aliase or user defined units */
class SimpleVoltageUnit(val name: String, val symbol: String, val interval: Real) extends VoltageUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultVoltageUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VoltageUnit
  
object VoltageUnitObjects{

  final case object volt extends SimpleVoltageUnit("volt", "V", 1)
  final case object yoctovolt extends SimpleVoltageUnit("yoctovolt", "yV", r"1e-24")
  final case object zeptovolt extends SimpleVoltageUnit("zeptovolt", "zV", r"1e-21")
  final case object attovolt extends SimpleVoltageUnit("attovolt", "aV", r"1e-18")
  final case object femtovolt extends SimpleVoltageUnit("femtovolt", "fV", r"1e-15")
  final case object picovolt extends SimpleVoltageUnit("picovolt", "pV", r"1e-12")
  final case object nanovolt extends SimpleVoltageUnit("nanovolt", "nV", r"1e-9")
  final case object microvolt extends DefaultVoltageUnit("microvolt", "μV", Seq("mcV"), r"1e-6")
  final case object millivolt extends SimpleVoltageUnit("millivolt", "mV", r"1e-3")
  final case object centivolt extends SimpleVoltageUnit("centivolt", "cV", r"1e-2")
  final case object decivolt extends SimpleVoltageUnit("decivolt", "dV", r"1e-1")
  final case object decavolt extends SimpleVoltageUnit("decavolt", "daV", r"1e1")
  final case object hectovolt extends SimpleVoltageUnit("hectovolt", "hV", r"1e2")
  final case object kilovolt extends DefaultVoltageUnit("kilovolt", "kV", Seq("KV"), r"1e3")
  final case object megavolt extends SimpleVoltageUnit("megavolt", "MV", r"1e6")
  final case object gigavolt extends SimpleVoltageUnit("gigavolt", "GV", r"1e9")
  final case object teravolt extends SimpleVoltageUnit("teravolt", "TV", r"1e12")
  final case object petavolt extends SimpleVoltageUnit("petavolt", "PV", r"1e15")
  final case object exavolt extends SimpleVoltageUnit("exavolt", "EV", r"1e18")
  final case object zettavolt extends SimpleVoltageUnit("zettavolt", "ZV", r"1e21")
  final case object yottavolt extends SimpleVoltageUnit("yottavolt", "YV", r"1e24")
  final case object abvolt extends SimpleVoltageUnit("abvolt", "abV", r"1e-8")
  final case object statvolt extends SimpleVoltageUnit("statvolt", "statV", Constants.SpeedOfLight * r"1e-6")
}


object VoltageUnits{

  /** volt */
  def V: VoltageUnit = VoltageUnitObjects.volt
  /** yoctovolt */
  def yV: VoltageUnit = VoltageUnitObjects.yoctovolt
  /** zeptovolt */
  def zV: VoltageUnit = VoltageUnitObjects.zeptovolt
  /** attovolt */
  def aV: VoltageUnit = VoltageUnitObjects.attovolt
  /** femtovolt */
  def fV: VoltageUnit = VoltageUnitObjects.femtovolt
  /** picovolt */
  def pV: VoltageUnit = VoltageUnitObjects.picovolt
  /** nanovolt */
  def nV: VoltageUnit = VoltageUnitObjects.nanovolt
  /** microvolt */
  def μV: VoltageUnit = VoltageUnitObjects.microvolt
  /** microvolt */
  def mcV: VoltageUnit = VoltageUnitObjects.microvolt
  /** millivolt */
  def mV: VoltageUnit = VoltageUnitObjects.millivolt
  /** centivolt */
  def cV: VoltageUnit = VoltageUnitObjects.centivolt
  /** decivolt */
  def dV: VoltageUnit = VoltageUnitObjects.decivolt
  /** decavolt */
  def daV: VoltageUnit = VoltageUnitObjects.decavolt
  /** hectovolt */
  def hV: VoltageUnit = VoltageUnitObjects.hectovolt
  /** kilovolt */
  def kV: VoltageUnit = VoltageUnitObjects.kilovolt
  /** kilovolt */
  def KV: VoltageUnit = VoltageUnitObjects.kilovolt
  /** megavolt */
  def MV: VoltageUnit = VoltageUnitObjects.megavolt
  /** gigavolt */
  def GV: VoltageUnit = VoltageUnitObjects.gigavolt
  /** teravolt */
  def TV: VoltageUnit = VoltageUnitObjects.teravolt
  /** petavolt */
  def PV: VoltageUnit = VoltageUnitObjects.petavolt
  /** exavolt */
  def EV: VoltageUnit = VoltageUnitObjects.exavolt
  /** zettavolt */
  def ZV: VoltageUnit = VoltageUnitObjects.zettavolt
  /** yottavolt */
  def YV: VoltageUnit = VoltageUnitObjects.yottavolt
  /** abvolt */
  def abV: VoltageUnit = VoltageUnitObjects.abvolt
  /** statvolt */
  def statV: VoltageUnit = VoltageUnitObjects.statvolt
}