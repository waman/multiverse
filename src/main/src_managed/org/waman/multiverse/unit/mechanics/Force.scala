package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit

import org.waman.multiverse.unit.basic.Length
import org.waman.multiverse.unit.basic.LengthUnit

import org.waman.multiverse.unit.basic.Area
import org.waman.multiverse.unit.basic.AreaUnit

import org.waman.multiverse.unit.fluid.Pressure
import org.waman.multiverse.unit.fluid.PressureUnit


class Force[A: Fractional](val value: A, val unit: ForceUnit)
    extends LinearQuantity[Force[A], A, ForceUnit] {

  override protected def newQuantity(value: A, unit: ForceUnit): Force[A] = new Force(value, unit)

  def *(time: Time[A]): Momentum[A] = new Momentum(this.value * time.value, this.unit * time.unit)

  def *(length: Length[A]): Torque[A] = new Torque(this.value * length.value, this.unit * length.unit)

  def /(area: Area[A]): Pressure[A] = new Pressure(this.value / area.value, this.unit / area.unit)
}

trait ForceUnit extends LinearUnit[ForceUnit]{

  override def getSIUnit: ForceUnit = ForceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ForceUnit.dimension

  def *(timeUnit: TimeUnit): MomentumUnit =
    new AbstractProductUnit[MomentumUnit, ForceUnit, TimeUnit](ForceUnit.this, timeUnit) with MomentumUnit

  def *(lengthUnit: LengthUnit): TorqueUnit =
    new AbstractProductUnit[TorqueUnit, ForceUnit, LengthUnit](ForceUnit.this, lengthUnit) with TorqueUnit

  def /(areaUnit: AreaUnit): PressureUnit =
    new AbstractQuotientUnit[PressureUnit, ForceUnit, AreaUnit](ForceUnit.this, areaUnit) with PressureUnit
}

object ForceUnit extends UnitInfo[ForceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> 1).withDefaultValue(0)

  def getSIUnit: ForceUnit = ForceUnitObjects.newton

  import ForceUnitObjects._
  def getUnits: Seq[ForceUnit] =
    Seq(newton, yoctonewton, zeptonewton, attonewton, femtonewton, piconewton, nanonewton, micronewton, millinewton, centinewton, decinewton, decanewton, hectonewton, kilonewton, meganewton, giganewton, teranewton, petanewton, exanewton, zettanewton, yottanewton, dyne, kilogram_force, milligrave_force, ounce_force, pound_force, poundal, kip_force, short_ton_force, long_ton_force, sthene)
}

/** For no aliase or user defined units */
class SimpleForceUnit(val name: String, val symbol: String, val interval: Real) extends ForceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultForceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ForceUnit

object ForceUnitObjects{
  import org.waman.multiverse.unit.mechanics.AccelerationUnitObjects._
  import org.waman.multiverse.unit.basic.MassUnitObjects._
  import org.waman.multiverse.unit.basic.LengthUnitObjects._

  final case object newton extends SimpleForceUnit("newton", "N", 1)
  final case object yoctonewton extends SimpleForceUnit("yoctonewton", "yN", r"1e-24")
  final case object zeptonewton extends SimpleForceUnit("zeptonewton", "zN", r"1e-21")
  final case object attonewton extends SimpleForceUnit("attonewton", "aN", r"1e-18")
  final case object femtonewton extends SimpleForceUnit("femtonewton", "fN", r"1e-15")
  final case object piconewton extends SimpleForceUnit("piconewton", "pN", r"1e-12")
  final case object nanonewton extends SimpleForceUnit("nanonewton", "nN", r"1e-9")
  final case object micronewton extends DefaultForceUnit("micronewton", "μN", Seq("mcN"), r"1e-6")
  final case object millinewton extends SimpleForceUnit("millinewton", "mN", r"1e-3")
  final case object centinewton extends SimpleForceUnit("centinewton", "cN", r"1e-2")
  final case object decinewton extends SimpleForceUnit("decinewton", "dN", r"1e-1")
  final case object decanewton extends SimpleForceUnit("decanewton", "daN", r"1e1")
  final case object hectonewton extends SimpleForceUnit("hectonewton", "hN", r"1e2")
  final case object kilonewton extends DefaultForceUnit("kilonewton", "kN", Seq("KN"), r"1e3")
  final case object meganewton extends SimpleForceUnit("meganewton", "MN", r"1e6")
  final case object giganewton extends SimpleForceUnit("giganewton", "GN", r"1e9")
  final case object teranewton extends SimpleForceUnit("teranewton", "TN", r"1e12")
  final case object petanewton extends SimpleForceUnit("petanewton", "PN", r"1e15")
  final case object exanewton extends SimpleForceUnit("exanewton", "EN", r"1e18")
  final case object zettanewton extends SimpleForceUnit("zettanewton", "ZN", r"1e21")
  final case object yottanewton extends SimpleForceUnit("yottanewton", "YN", r"1e24")
  final case object dyne extends SimpleForceUnit("dyne", "dyn", r"1e-5")
  final case object kilogram_force extends DefaultForceUnit("kilogram force", "kgf", Seq("kp", "Gf"), standard_gravity.interval) with NotExact
  final case object milligrave_force extends DefaultForceUnit("milligrave force", "mGf", Seq("gf"), r"1e-3" * standard_gravity.interval) with NotExact
  final case object ounce_force extends SimpleForceUnit("ounce force", "ozf", ounce.interval * standard_gravity.interval)
  final case object pound_force extends SimpleForceUnit("pound force", "lbf", pound.interval * standard_gravity.interval)
  final case object poundal extends SimpleForceUnit("poundal", "pdl", pound.interval * foot.interval)
  final case object kip_force extends DefaultForceUnit("kip force", "kipf", Seq("klbf"), r"1000" * pound.interval * standard_gravity.interval)
  final case object short_ton_force extends SimpleForceUnit("short ton force", "sh_tnf", short_ton.interval * standard_gravity.interval)
  final case object long_ton_force extends DefaultForceUnit("long ton force", "tnf", Seq("long_tnf"), long_ton.interval * standard_gravity.interval)
  final case object sthene extends SimpleForceUnit("sthene", "sn", r"1e3")
}

object ForceUnits{
  def N: ForceUnit = ForceUnitObjects.newton
  def yN: ForceUnit = ForceUnitObjects.yoctonewton
  def zN: ForceUnit = ForceUnitObjects.zeptonewton
  def aN: ForceUnit = ForceUnitObjects.attonewton
  def fN: ForceUnit = ForceUnitObjects.femtonewton
  def pN: ForceUnit = ForceUnitObjects.piconewton
  def nN: ForceUnit = ForceUnitObjects.nanonewton
  def `μN`: ForceUnit = ForceUnitObjects.micronewton
  def mcN: ForceUnit = ForceUnitObjects.micronewton
  def mN: ForceUnit = ForceUnitObjects.millinewton
  def cN: ForceUnit = ForceUnitObjects.centinewton
  def dN: ForceUnit = ForceUnitObjects.decinewton
  def daN: ForceUnit = ForceUnitObjects.decanewton
  def hN: ForceUnit = ForceUnitObjects.hectonewton
  def kN: ForceUnit = ForceUnitObjects.kilonewton
  def KN: ForceUnit = ForceUnitObjects.kilonewton
  def MN: ForceUnit = ForceUnitObjects.meganewton
  def GN: ForceUnit = ForceUnitObjects.giganewton
  def TN: ForceUnit = ForceUnitObjects.teranewton
  def PN: ForceUnit = ForceUnitObjects.petanewton
  def EN: ForceUnit = ForceUnitObjects.exanewton
  def ZN: ForceUnit = ForceUnitObjects.zettanewton
  def YN: ForceUnit = ForceUnitObjects.yottanewton
  def dyn: ForceUnit = ForceUnitObjects.dyne
  def kgf: ForceUnit = ForceUnitObjects.kilogram_force
  def kp: ForceUnit = ForceUnitObjects.kilogram_force
  def Gf: ForceUnit = ForceUnitObjects.kilogram_force
  def mGf: ForceUnit = ForceUnitObjects.milligrave_force
  def gf: ForceUnit = ForceUnitObjects.milligrave_force
  def ozf: ForceUnit = ForceUnitObjects.ounce_force
  def lbf: ForceUnit = ForceUnitObjects.pound_force
  def pdl: ForceUnit = ForceUnitObjects.poundal
  def kipf: ForceUnit = ForceUnitObjects.kip_force
  def klbf: ForceUnit = ForceUnitObjects.kip_force
  def sh_tnf: ForceUnit = ForceUnitObjects.short_ton_force
  def tnf: ForceUnit = ForceUnitObjects.long_ton_force
  def long_tnf: ForceUnit = ForceUnitObjects.long_ton_force
  def sn: ForceUnit = ForceUnitObjects.sthene
}