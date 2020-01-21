package org.waman.multiverse.unit.electrics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit
import org.waman.multiverse.unit.magnetics.Flux
import org.waman.multiverse.unit.magnetics.FluxUnit

class Voltage[A: Fractional](val value: A, val unit: VoltageUnit)
    extends LinearQuantity[Voltage[A], A, VoltageUnit] {

  override protected def newQuantity(value: A, unit: VoltageUnit): Voltage[A] = new Voltage(value, unit)

  def *(time: Time[A]): Flux[A] = new Flux(this.value * time.value, this.unit * time.unit)

  def /(current: Current[A]): Resistance[A] = new Resistance(this.value / current.value, this.unit / current.unit)

}

trait VoltageUnit extends LinearUnit[VoltageUnit]{

  override def getSIUnit: VoltageUnit = VoltageUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VoltageUnit.dimension

  def *(timeUnit: TimeUnit): FluxUnit =
    new AbstractProductUnit[FluxUnit, VoltageUnit, TimeUnit](VoltageUnit.this, timeUnit) with FluxUnit

  def /(currentUnit: CurrentUnit): ResistanceUnit =
    new AbstractQuotientUnit[ResistanceUnit, VoltageUnit, CurrentUnit](VoltageUnit.this, currentUnit) with ResistanceUnit

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

/** For user defined units */
class SimpleVoltageUnit(val name: String, val symbol: String, val interval: Real) extends VoltageUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultVoltageUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VoltageUnit

object VoltageUnitObjects{
  import org.waman.multiverse.unit.Constants

  final case object volt extends DefaultVoltageUnit("volt", "V", Nil, 1)
  final case object yoctovolt extends DefaultVoltageUnit("yoctovolt", "yV", Nil, r"1e-24")
  final case object zeptovolt extends DefaultVoltageUnit("zeptovolt", "zV", Nil, r"1e-21")
  final case object attovolt extends DefaultVoltageUnit("attovolt", "aV", Nil, r"1e-18")
  final case object femtovolt extends DefaultVoltageUnit("femtovolt", "fV", Nil, r"1e-15")
  final case object picovolt extends DefaultVoltageUnit("picovolt", "pV", Nil, r"1e-12")
  final case object nanovolt extends DefaultVoltageUnit("nanovolt", "nV", Nil, r"1e-9")
  final case object microvolt extends DefaultVoltageUnit("microvolt", "μV", Seq("mcV"), r"1e-6")
  final case object millivolt extends DefaultVoltageUnit("millivolt", "mV", Nil, r"1e-3")
  final case object centivolt extends DefaultVoltageUnit("centivolt", "cV", Nil, r"1e-2")
  final case object decivolt extends DefaultVoltageUnit("decivolt", "dV", Nil, r"1e-1")
  final case object decavolt extends DefaultVoltageUnit("decavolt", "daV", Nil, r"1e1")
  final case object hectovolt extends DefaultVoltageUnit("hectovolt", "hV", Nil, r"1e2")
  final case object kilovolt extends DefaultVoltageUnit("kilovolt", "kV", Seq("KV"), r"1e3")
  final case object megavolt extends DefaultVoltageUnit("megavolt", "MV", Nil, r"1e6")
  final case object gigavolt extends DefaultVoltageUnit("gigavolt", "GV", Nil, r"1e9")
  final case object teravolt extends DefaultVoltageUnit("teravolt", "TV", Nil, r"1e12")
  final case object petavolt extends DefaultVoltageUnit("petavolt", "PV", Nil, r"1e15")
  final case object exavolt extends DefaultVoltageUnit("exavolt", "EV", Nil, r"1e18")
  final case object zettavolt extends DefaultVoltageUnit("zettavolt", "ZV", Nil, r"1e21")
  final case object yottavolt extends DefaultVoltageUnit("yottavolt", "YV", Nil, r"1e24")
  final case object statvolt extends DefaultVoltageUnit("statvolt", "statV", Nil, Constants.SpeedOfLight / r"1e6")
  final case object abvolt extends DefaultVoltageUnit("abvolt", "abV", Nil, r"1e-8")
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