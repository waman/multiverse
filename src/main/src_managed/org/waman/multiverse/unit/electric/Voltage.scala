package org.waman.multiverse.unit.electric

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit

import org.waman.multiverse.unit.magnetic.Flux
import org.waman.multiverse.unit.magnetic.FluxUnit




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

object VoltageUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, I -> -1, L -> 2).withDefaultValue(0)

  def getSIUnit: VoltageUnit = VoltageUnitObjects.volt

  import VoltageUnitObjects._
  def getUnits: Seq[VoltageUnit] =
    Seq(volt, yoctovolt, zeptovolt, attovolt, femtovolt, picovolt, nanovolt, microvolt, millivolt, centivolt, decivolt, decavolt, hectovolt, kilovolt, megavolt, gigavolt, teravolt, petavolt, exavolt, zettavolt, yottavolt, statvolt, abvolt)
}

class DefaultVoltageUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VoltageUnit

object VoltageUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object volt extends DefaultVoltageUnit("volt", "V", Nil, 1)
  final object yoctovolt extends DefaultVoltageUnit("yoctovolt", "yV", Nil, 1 * r"1e-24")
  final object zeptovolt extends DefaultVoltageUnit("zeptovolt", "zV", Nil, 1 * r"1e-21")
  final object attovolt extends DefaultVoltageUnit("attovolt", "aV", Nil, 1 * r"1e-18")
  final object femtovolt extends DefaultVoltageUnit("femtovolt", "fV", Nil, 1 * r"1e-15")
  final object picovolt extends DefaultVoltageUnit("picovolt", "pV", Nil, 1 * r"1e-12")
  final object nanovolt extends DefaultVoltageUnit("nanovolt", "nV", Nil, 1 * r"1e-9")
  final object microvolt extends DefaultVoltageUnit("microvolt", "μV", Seq("mcV"), 1 * r"1e-6")
  final object millivolt extends DefaultVoltageUnit("millivolt", "mV", Nil, 1 * r"1e-3")
  final object centivolt extends DefaultVoltageUnit("centivolt", "cV", Nil, 1 * r"1e-2")
  final object decivolt extends DefaultVoltageUnit("decivolt", "dV", Nil, 1 * r"1e-1")
  final object decavolt extends DefaultVoltageUnit("decavolt", "daV", Nil, 1 * r"1e1")
  final object hectovolt extends DefaultVoltageUnit("hectovolt", "hV", Nil, 1 * r"1e2")
  final object kilovolt extends DefaultVoltageUnit("kilovolt", "kV", Seq("KV"), 1 * r"1e3")
  final object megavolt extends DefaultVoltageUnit("megavolt", "MV", Nil, 1 * r"1e6")
  final object gigavolt extends DefaultVoltageUnit("gigavolt", "GV", Nil, 1 * r"1e9")
  final object teravolt extends DefaultVoltageUnit("teravolt", "TV", Nil, 1 * r"1e12")
  final object petavolt extends DefaultVoltageUnit("petavolt", "PV", Nil, 1 * r"1e15")
  final object exavolt extends DefaultVoltageUnit("exavolt", "EV", Nil, 1 * r"1e18")
  final object zettavolt extends DefaultVoltageUnit("zettavolt", "ZV", Nil, 1 * r"1e21")
  final object yottavolt extends DefaultVoltageUnit("yottavolt", "YV", Nil, 1 * r"1e24")
  final object statvolt extends DefaultVoltageUnit("statvolt", "statV", Nil, Constants.SpeedOfLight / r"1e6")
  final object abvolt extends DefaultVoltageUnit("abvolt", "abV", Nil, r"1e-8")
}

object VoltageUnits{
  def V: VoltageUnit = VoltageUnitObjects.volt
  def yV: VoltageUnit = VoltageUnitObjects.yoctovolt
  def zV: VoltageUnit = VoltageUnitObjects.zeptovolt
  def aV: VoltageUnit = VoltageUnitObjects.attovolt
  def fV: VoltageUnit = VoltageUnitObjects.femtovolt
  def pV: VoltageUnit = VoltageUnitObjects.picovolt
  def nV: VoltageUnit = VoltageUnitObjects.nanovolt
  def μV: VoltageUnit = VoltageUnitObjects.microvolt
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