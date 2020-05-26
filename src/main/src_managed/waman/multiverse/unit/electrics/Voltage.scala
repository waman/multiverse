package waman.multiverse.unit.electrics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


import waman.multiverse.unit.basic.Time
import waman.multiverse.unit.basic.TimeUnit


import waman.multiverse.unit.magnetics.Flux
import waman.multiverse.unit.magnetics.FluxUnit


class Voltage[A: Fractional](val value: A, val unit: VoltageUnit)
    extends LinearQuantity[Voltage[A], A, VoltageUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: VoltageUnit): Voltage[A] = new Voltage(value, unit)

  def *(time: Time[A]): Flux[A] = new Flux(this.value * time.value, this.unit * time.unit)

  def /(current: Current[A]): Resistance[A] = new Resistance(this.value / current.value, this.unit / current.unit)
}

trait VoltageUnit extends LinearUnit[VoltageUnit]{

  override def getSIUnit: VoltageUnit = VoltageUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VoltageUnit.dimension

  def *(timeUnit: TimeUnit): FluxUnit =
    new ProductUnit[FluxUnit, VoltageUnit, TimeUnit](VoltageUnit.this, timeUnit) with FluxUnit

  def /(currentUnit: CurrentUnit): ResistanceUnit =
    new QuotientUnit[ResistanceUnit, VoltageUnit, CurrentUnit](VoltageUnit.this, currentUnit) with ResistanceUnit
}

object VoltageUnit extends UnitInfo[VoltageUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, I -> -1, L -> 2).withDefaultValue(0)

  def getSIUnit: VoltageUnit = VoltageUnitObjects.volt

  import VoltageUnitObjects._
  def getUnits: Seq[VoltageUnit] =
    Seq(volt, yoctovolt, zeptovolt, attovolt, femtovolt, picovolt, nanovolt, microvolt, millivolt, centivolt, decivolt, decavolt, hectovolt, kilovolt, megavolt, gigavolt, teravolt, petavolt, exavolt, zettavolt, yottavolt, statvolt, abvolt)
}

/** For no aliase or user defined units */
class SimpleVoltageUnit(val name: String, val symbol: String, val interval: Real) extends VoltageUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultVoltageUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VoltageUnit

object VoltageUnitObjects{

  import spire.implicits._

  import waman.multiverse.unit.Constants

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
  final case object statvolt extends SimpleVoltageUnit("statvolt", "statV", Constants.SpeedOfLight / r"1e6")
  final case object abvolt extends SimpleVoltageUnit("abvolt", "abV", r"1e-8")
}

object VoltageUnits{

  def V: VoltageUnit = VoltageUnitObjects.volt
  def yV: VoltageUnit = VoltageUnitObjects.yoctovolt
  def zV: VoltageUnit = VoltageUnitObjects.zeptovolt
  def aV: VoltageUnit = VoltageUnitObjects.attovolt
  def fV: VoltageUnit = VoltageUnitObjects.femtovolt
  def pV: VoltageUnit = VoltageUnitObjects.picovolt
  def nV: VoltageUnit = VoltageUnitObjects.nanovolt
  def `μV`: VoltageUnit = VoltageUnitObjects.microvolt
  def mcV: VoltageUnit = VoltageUnitObjects.microvolt
  def mV: VoltageUnit = VoltageUnitObjects.millivolt
  def cV: VoltageUnit = VoltageUnitObjects.centivolt
  def dV: VoltageUnit = VoltageUnitObjects.decivolt
  def daV: VoltageUnit = VoltageUnitObjects.decavolt
  def hV: VoltageUnit = VoltageUnitObjects.hectovolt
  def kV: VoltageUnit = VoltageUnitObjects.kilovolt
  def KV: VoltageUnit = VoltageUnitObjects.kilovolt
  def MV: VoltageUnit = VoltageUnitObjects.megavolt
  def GV: VoltageUnit = VoltageUnitObjects.gigavolt
  def TV: VoltageUnit = VoltageUnitObjects.teravolt
  def PV: VoltageUnit = VoltageUnitObjects.petavolt
  def EV: VoltageUnit = VoltageUnitObjects.exavolt
  def ZV: VoltageUnit = VoltageUnitObjects.zettavolt
  def YV: VoltageUnit = VoltageUnitObjects.yottavolt
  def statV: VoltageUnit = VoltageUnitObjects.statvolt
  def abV: VoltageUnit = VoltageUnitObjects.abvolt
}