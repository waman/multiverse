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

object ForceUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> 1).withDefaultValue(0)

  def getSIUnit: ForceUnit = ForceUnitObjects.newton

  import ForceUnitObjects._
  def getUnits: Seq[ForceUnit] =
    Seq(newton, yoctonewton, zeptonewton, attonewton, femtonewton, piconewton, nanonewton, micronewton, millinewton, centinewton, decinewton, decanewton, hectonewton, kilonewton, meganewton, giganewton, teranewton, petanewton, exanewton, zettanewton, yottanewton, dyne, kilogram_force, milligrave_force, ounce_force, pound_force, poundal, kip_force, short_ton_force, long_ton_force, sthene)
}

class DefaultForceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ForceUnit

object ForceUnitObjects{
  import org.waman.multiverse.unit.basic.MassUnitObjects
  import org.waman.multiverse.unit.basic.LengthUnitObjects

  final object newton extends DefaultForceUnit("newton", "N", Nil, 1)
  final object yoctonewton extends DefaultForceUnit("yoctonewton", "yN", Nil, 1 * r"1e-24")
  final object zeptonewton extends DefaultForceUnit("zeptonewton", "zN", Nil, 1 * r"1e-21")
  final object attonewton extends DefaultForceUnit("attonewton", "aN", Nil, 1 * r"1e-18")
  final object femtonewton extends DefaultForceUnit("femtonewton", "fN", Nil, 1 * r"1e-15")
  final object piconewton extends DefaultForceUnit("piconewton", "pN", Nil, 1 * r"1e-12")
  final object nanonewton extends DefaultForceUnit("nanonewton", "nN", Nil, 1 * r"1e-9")
  final object micronewton extends DefaultForceUnit("micronewton", "μN", Seq("mcN"), 1 * r"1e-6")
  final object millinewton extends DefaultForceUnit("millinewton", "mN", Nil, 1 * r"1e-3")
  final object centinewton extends DefaultForceUnit("centinewton", "cN", Nil, 1 * r"1e-2")
  final object decinewton extends DefaultForceUnit("decinewton", "dN", Nil, 1 * r"1e-1")
  final object decanewton extends DefaultForceUnit("decanewton", "daN", Nil, 1 * r"1e1")
  final object hectonewton extends DefaultForceUnit("hectonewton", "hN", Nil, 1 * r"1e2")
  final object kilonewton extends DefaultForceUnit("kilonewton", "kN", Seq("KN"), 1 * r"1e3")
  final object meganewton extends DefaultForceUnit("meganewton", "MN", Nil, 1 * r"1e6")
  final object giganewton extends DefaultForceUnit("giganewton", "GN", Nil, 1 * r"1e9")
  final object teranewton extends DefaultForceUnit("teranewton", "TN", Nil, 1 * r"1e12")
  final object petanewton extends DefaultForceUnit("petanewton", "PN", Nil, 1 * r"1e15")
  final object exanewton extends DefaultForceUnit("exanewton", "EN", Nil, 1 * r"1e18")
  final object zettanewton extends DefaultForceUnit("zettanewton", "ZN", Nil, 1 * r"1e21")
  final object yottanewton extends DefaultForceUnit("yottanewton", "YN", Nil, 1 * r"1e24")
  final object dyne extends DefaultForceUnit("dyne", "dyn", Nil, r"1e-5")
  final object kilogram_force extends DefaultForceUnit("kilogram force", "kgf", Seq("kp", "Gf"), AccelerationUnitObjects.standard_gravity.interval) with NotExact
  final object milligrave_force extends DefaultForceUnit("milligrave force", "mGf", Seq("gf"), r"1e-3" * AccelerationUnitObjects.standard_gravity.interval) with NotExact
  final object ounce_force extends DefaultForceUnit("ounce force", "ozf", Nil, MassUnitObjects.ounce.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object pound_force extends DefaultForceUnit("pound force", "lbf", Nil, MassUnitObjects.pound.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object poundal extends DefaultForceUnit("poundal", "pdl", Nil, MassUnitObjects.pound.interval * LengthUnitObjects.foot.interval)
  final object kip_force extends DefaultForceUnit("kip force", "kipf", Seq("klbf"), r"1000" * MassUnitObjects.pound.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object short_ton_force extends DefaultForceUnit("short ton force", "sh_tnf", Nil, MassUnitObjects.short_ton.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object long_ton_force extends DefaultForceUnit("long ton force", "tnf", Seq("long_tnf"), MassUnitObjects.long_ton.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object sthene extends DefaultForceUnit("sthene", "sn", Nil, r"1e3")
}

object ForceUnits{
  def N: ForceUnit = ForceUnitObjects.newton
  def yN: ForceUnit = ForceUnitObjects.yoctonewton
  def zN: ForceUnit = ForceUnitObjects.zeptonewton
  def aN: ForceUnit = ForceUnitObjects.attonewton
  def fN: ForceUnit = ForceUnitObjects.femtonewton
  def pN: ForceUnit = ForceUnitObjects.piconewton
  def nN: ForceUnit = ForceUnitObjects.nanonewton
  def μN: ForceUnit = ForceUnitObjects.micronewton
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