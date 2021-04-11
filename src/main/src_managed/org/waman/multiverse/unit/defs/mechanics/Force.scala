package org.waman.multiverse.unit.defs.mechanics

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.fluid._

class Force[A: Fractional](val value: A, val unit: ForceUnit)
    extends LinearQuantity[Force[A], A, ForceUnit] {

  override protected def newQuantity(value: A, unit: ForceUnit): Force[A] = new Force(value, unit)

  def /(magneticFlux: MagneticFlux[A]): MagneticFieldStrength[A] = new MagneticFieldStrength(this.value / magneticFlux.value, this.unit / magneticFlux.unit)

  def /(area: Area[A]): Pressure[A] = new Pressure(this.value / area.value, this.unit / area.unit)

  def *(length: Length[A]): Energy[A] = new Energy(this.value * length.value, this.unit * length.unit)

  def *(time: Time[A]): Momentum[A] = new Momentum(this.value * time.value, this.unit * time.unit)
}

trait ForceUnit extends LinearUnit[ForceUnit]{

  override def getSIUnit: ForceUnit = ForceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ForceUnit.dimension

  def /(magneticFluxUnit: MagneticFluxUnit): MagneticFieldStrengthUnit =
    new QuotientUnit[MagneticFieldStrengthUnit, ForceUnit, MagneticFluxUnit](ForceUnit.this, magneticFluxUnit) with MagneticFieldStrengthUnit

  def /(areaUnit: AreaUnit): PressureUnit =
    new QuotientUnit[PressureUnit, ForceUnit, AreaUnit](ForceUnit.this, areaUnit) with PressureUnit

  def *(lengthUnit: LengthUnit): EnergyUnit =
    new ProductUnit[EnergyUnit, ForceUnit, LengthUnit](ForceUnit.this, lengthUnit) with EnergyUnit

  def *(timeUnit: TimeUnit): MomentumUnit =
    new ProductUnit[MomentumUnit, ForceUnit, TimeUnit](ForceUnit.this, timeUnit) with MomentumUnit
}

object ForceUnit extends UnitInfo[ForceUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> 1).withDefaultValue(0)

  def getSIUnit: ForceUnit = ForceUnitObjects.newton
  import ForceUnitObjects._

  def getUnits: Seq[ForceUnit] =
    Seq(newton, yoctonewton, zeptonewton, attonewton, femtonewton, piconewton, nanonewton, micronewton, millinewton, centinewton, decinewton, decanewton, hectonewton, kilonewton, meganewton, giganewton, teranewton, petanewton, exanewton, zettanewton, yottanewton, dyne, yoctodyne, zeptodyne, attodyne, femtodyne, picodyne, nanodyne, microdyne, millidyne, centidyne, decidyne, decadyne, hectodyne, kilodyne, megadyne, gigadyne, teradyne, petadyne, exadyne, zettadyne, yottadyne, kilogram_force, milligrave_force, atomic_unit_of_force, ounce_force, pound_force, poundal, kip_force, short_ton_force, long_ton_force, sthene)
}


/** For no aliase or user defined units */
class SimpleForceUnit(val name: String, val symbol: String, val interval: Real) extends ForceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultForceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ForceUnit
  
object ForceUnitObjects{

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
  final case object yoctodyne extends SimpleForceUnit("yoctodyne", "ydyn", r"1e-5" * r"1e-24")
  final case object zeptodyne extends SimpleForceUnit("zeptodyne", "zdyn", r"1e-5" * r"1e-21")
  final case object attodyne extends SimpleForceUnit("attodyne", "adyn", r"1e-5" * r"1e-18")
  final case object femtodyne extends SimpleForceUnit("femtodyne", "fdyn", r"1e-5" * r"1e-15")
  final case object picodyne extends SimpleForceUnit("picodyne", "pdyn", r"1e-5" * r"1e-12")
  final case object nanodyne extends SimpleForceUnit("nanodyne", "ndyn", r"1e-5" * r"1e-9")
  final case object microdyne extends DefaultForceUnit("microdyne", "μdyn", Seq("mcdyn"), r"1e-5" * r"1e-6")
  final case object millidyne extends SimpleForceUnit("millidyne", "mdyn", r"1e-5" * r"1e-3")
  final case object centidyne extends SimpleForceUnit("centidyne", "cdyn", r"1e-5" * r"1e-2")
  final case object decidyne extends SimpleForceUnit("decidyne", "ddyn", r"1e-5" * r"1e-1")
  final case object decadyne extends SimpleForceUnit("decadyne", "dadyn", r"1e-5" * r"1e1")
  final case object hectodyne extends SimpleForceUnit("hectodyne", "hdyn", r"1e-5" * r"1e2")
  final case object kilodyne extends DefaultForceUnit("kilodyne", "kdyn", Seq("Kdyn"), r"1e-5" * r"1e3")
  final case object megadyne extends SimpleForceUnit("megadyne", "Mdyn", r"1e-5" * r"1e6")
  final case object gigadyne extends SimpleForceUnit("gigadyne", "Gdyn", r"1e-5" * r"1e9")
  final case object teradyne extends SimpleForceUnit("teradyne", "Tdyn", r"1e-5" * r"1e12")
  final case object petadyne extends SimpleForceUnit("petadyne", "Pdyn", r"1e-5" * r"1e15")
  final case object exadyne extends SimpleForceUnit("exadyne", "Edyn", r"1e-5" * r"1e18")
  final case object zettadyne extends SimpleForceUnit("zettadyne", "Zdyn", r"1e-5" * r"1e21")
  final case object yottadyne extends SimpleForceUnit("yottadyne", "Ydyn", r"1e-5" * r"1e24")
  final case object kilogram_force extends DefaultForceUnit("kilogram force", "kgf", Seq("kp", "Gf"), AccelerationUnitObjects.standard_gravity.interval) with NotExact
  final case object milligrave_force extends DefaultForceUnit("milligrave force", "mGf", Seq("gf"), r"1e-3" * AccelerationUnitObjects.standard_gravity.interval) with NotExact
  final case object atomic_unit_of_force extends SimpleForceUnit("atomic unit of force", "atomic_unit_of_force", r"8.23872206e-8") with NotExact
  final case object ounce_force extends SimpleForceUnit("ounce force", "ozf", MassUnitObjects.ounce.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object pound_force extends SimpleForceUnit("pound force", "lbf", MassUnitObjects.pound.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object poundal extends SimpleForceUnit("poundal", "pdl", MassUnitObjects.pound.interval * LengthUnitObjects.foot.interval)
  final case object kip_force extends DefaultForceUnit("kip force", "kipf", Seq("klbf"), r"1000" * MassUnitObjects.pound.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object short_ton_force extends SimpleForceUnit("short ton force", "sh_tnf", MassUnitObjects.short_ton.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object long_ton_force extends DefaultForceUnit("long ton force", "tnf", Seq("long_tnf"), MassUnitObjects.long_ton.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object sthene extends SimpleForceUnit("sthene", "sn", r"1e3")
}


object ForceUnits{

  /** newton */
  def N: ForceUnit = ForceUnitObjects.newton
  /** yoctonewton */
  def yN: ForceUnit = ForceUnitObjects.yoctonewton
  /** zeptonewton */
  def zN: ForceUnit = ForceUnitObjects.zeptonewton
  /** attonewton */
  def aN: ForceUnit = ForceUnitObjects.attonewton
  /** femtonewton */
  def fN: ForceUnit = ForceUnitObjects.femtonewton
  /** piconewton */
  def pN: ForceUnit = ForceUnitObjects.piconewton
  /** nanonewton */
  def nN: ForceUnit = ForceUnitObjects.nanonewton
  /** micronewton */
  def μN: ForceUnit = ForceUnitObjects.micronewton
  /** micronewton */
  def mcN: ForceUnit = ForceUnitObjects.micronewton
  /** millinewton */
  def mN: ForceUnit = ForceUnitObjects.millinewton
  /** centinewton */
  def cN: ForceUnit = ForceUnitObjects.centinewton
  /** decinewton */
  def dN: ForceUnit = ForceUnitObjects.decinewton
  /** decanewton */
  def daN: ForceUnit = ForceUnitObjects.decanewton
  /** hectonewton */
  def hN: ForceUnit = ForceUnitObjects.hectonewton
  /** kilonewton */
  def kN: ForceUnit = ForceUnitObjects.kilonewton
  /** kilonewton */
  def KN: ForceUnit = ForceUnitObjects.kilonewton
  /** meganewton */
  def MN: ForceUnit = ForceUnitObjects.meganewton
  /** giganewton */
  def GN: ForceUnit = ForceUnitObjects.giganewton
  /** teranewton */
  def TN: ForceUnit = ForceUnitObjects.teranewton
  /** petanewton */
  def PN: ForceUnit = ForceUnitObjects.petanewton
  /** exanewton */
  def EN: ForceUnit = ForceUnitObjects.exanewton
  /** zettanewton */
  def ZN: ForceUnit = ForceUnitObjects.zettanewton
  /** yottanewton */
  def YN: ForceUnit = ForceUnitObjects.yottanewton
  /** dyne */
  def dyn: ForceUnit = ForceUnitObjects.dyne
  /** yoctodyne */
  def ydyn: ForceUnit = ForceUnitObjects.yoctodyne
  /** zeptodyne */
  def zdyn: ForceUnit = ForceUnitObjects.zeptodyne
  /** attodyne */
  def adyn: ForceUnit = ForceUnitObjects.attodyne
  /** femtodyne */
  def fdyn: ForceUnit = ForceUnitObjects.femtodyne
  /** picodyne */
  def pdyn: ForceUnit = ForceUnitObjects.picodyne
  /** nanodyne */
  def ndyn: ForceUnit = ForceUnitObjects.nanodyne
  /** microdyne */
  def μdyn: ForceUnit = ForceUnitObjects.microdyne
  /** microdyne */
  def mcdyn: ForceUnit = ForceUnitObjects.microdyne
  /** millidyne */
  def mdyn: ForceUnit = ForceUnitObjects.millidyne
  /** centidyne */
  def cdyn: ForceUnit = ForceUnitObjects.centidyne
  /** decidyne */
  def ddyn: ForceUnit = ForceUnitObjects.decidyne
  /** decadyne */
  def dadyn: ForceUnit = ForceUnitObjects.decadyne
  /** hectodyne */
  def hdyn: ForceUnit = ForceUnitObjects.hectodyne
  /** kilodyne */
  def kdyn: ForceUnit = ForceUnitObjects.kilodyne
  /** kilodyne */
  def Kdyn: ForceUnit = ForceUnitObjects.kilodyne
  /** megadyne */
  def Mdyn: ForceUnit = ForceUnitObjects.megadyne
  /** gigadyne */
  def Gdyn: ForceUnit = ForceUnitObjects.gigadyne
  /** teradyne */
  def Tdyn: ForceUnit = ForceUnitObjects.teradyne
  /** petadyne */
  def Pdyn: ForceUnit = ForceUnitObjects.petadyne
  /** exadyne */
  def Edyn: ForceUnit = ForceUnitObjects.exadyne
  /** zettadyne */
  def Zdyn: ForceUnit = ForceUnitObjects.zettadyne
  /** yottadyne */
  def Ydyn: ForceUnit = ForceUnitObjects.yottadyne
  /** kilogram force */
  def kgf: ForceUnit = ForceUnitObjects.kilogram_force
  /** kilogram force */
  def kp: ForceUnit = ForceUnitObjects.kilogram_force
  /** kilogram force */
  def Gf: ForceUnit = ForceUnitObjects.kilogram_force
  /** milligrave force */
  def mGf: ForceUnit = ForceUnitObjects.milligrave_force
  /** milligrave force */
  def gf: ForceUnit = ForceUnitObjects.milligrave_force
  /** atomic unit of force */
  def atomic_unit_of_force: ForceUnit = ForceUnitObjects.atomic_unit_of_force
  /** ounce force */
  def ozf: ForceUnit = ForceUnitObjects.ounce_force
  /** pound force */
  def lbf: ForceUnit = ForceUnitObjects.pound_force
  /** poundal */
  def pdl: ForceUnit = ForceUnitObjects.poundal
  /** kip force */
  def kipf: ForceUnit = ForceUnitObjects.kip_force
  /** kip force */
  def klbf: ForceUnit = ForceUnitObjects.kip_force
  /** short ton force */
  def sh_tnf: ForceUnit = ForceUnitObjects.short_ton_force
  /** long ton force */
  def tnf: ForceUnit = ForceUnitObjects.long_ton_force
  /** long ton force */
  def long_tnf: ForceUnit = ForceUnitObjects.long_ton_force
  /** sthene */
  def sn: ForceUnit = ForceUnitObjects.sthene
}