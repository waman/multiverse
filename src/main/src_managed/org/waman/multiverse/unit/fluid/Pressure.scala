package org.waman.multiverse.unit.fluid

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit



class Pressure[A: Fractional](val value: A, val unit: PressureUnit)
    extends LinearQuantity[Pressure[A], A, PressureUnit] {

  override protected def newQuantity(value: A, unit: PressureUnit): Pressure[A] = new Pressure(value, unit)

  def *(time: Time[A]): DynamicViscosity[A] = new DynamicViscosity(this.value * time.value, this.unit * time.unit)

}

trait PressureUnit extends LinearUnit[PressureUnit]{

  override def getSIUnit: PressureUnit = PressureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = PressureUnit.dimension

  def *(timeUnit: TimeUnit): DynamicViscosityUnit =
    new AbstractProductUnit[DynamicViscosityUnit, PressureUnit, TimeUnit](PressureUnit.this, timeUnit) with DynamicViscosityUnit

}

object PressureUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> -1).withDefaultValue(0)

  def getSIUnit: PressureUnit = PressureUnitObjects.pascal

  import PressureUnitObjects._
  def getUnits: Seq[PressureUnit] =
    Seq(pascal, yoctopascal, zeptopascal, attopascal, femtopascal, picopascal, nanopascal, micropascal, millipascal, centipascal, decipascal, decapascal, hectopascal, kilopascal, megapascal, gigapascal, terapascal, petapascal, exapascal, zettapascal, yottapascal, barye, atmosphere, atmosphere_technical, bar, pieze, torr, kip_per_square_inch, pound_per_square_foot, pound_per_square_inch, micrometre_of_mercury, millimetre_of_mercury, centimetre_of_mercury, inch_of_mercury, foot_of_mercury, millimetre_of_water, centimetre_of_water, inch_of_water, foot_of_water)
}

class DefaultPressureUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends PressureUnit

object PressureUnitObjects{
  import org.waman.multiverse.unit.mechanics.ForceUnitObjects
  import org.waman.multiverse.unit.basic.AreaUnitObjects
  import org.waman.multiverse.unit.basic.DensityUnitObjects
  import org.waman.multiverse.unit.basic.LengthUnitObjects
  import org.waman.multiverse.unit.mechanics.AccelerationUnitObjects

  final object pascal extends DefaultPressureUnit("pascal", "Pa", Nil, 1)
  final object yoctopascal extends DefaultPressureUnit("yoctopascal", "yPa", Nil, 1 * r"1e-24")
  final object zeptopascal extends DefaultPressureUnit("zeptopascal", "zPa", Nil, 1 * r"1e-21")
  final object attopascal extends DefaultPressureUnit("attopascal", "aPa", Nil, 1 * r"1e-18")
  final object femtopascal extends DefaultPressureUnit("femtopascal", "fPa", Nil, 1 * r"1e-15")
  final object picopascal extends DefaultPressureUnit("picopascal", "pPa", Nil, 1 * r"1e-12")
  final object nanopascal extends DefaultPressureUnit("nanopascal", "nPa", Nil, 1 * r"1e-9")
  final object micropascal extends DefaultPressureUnit("micropascal", "μPa", Seq("mcPa"), 1 * r"1e-6")
  final object millipascal extends DefaultPressureUnit("millipascal", "mPa", Nil, 1 * r"1e-3")
  final object centipascal extends DefaultPressureUnit("centipascal", "cPa", Nil, 1 * r"1e-2")
  final object decipascal extends DefaultPressureUnit("decipascal", "dPa", Nil, 1 * r"1e-1")
  final object decapascal extends DefaultPressureUnit("decapascal", "daPa", Nil, 1 * r"1e1")
  final object hectopascal extends DefaultPressureUnit("hectopascal", "hPa", Nil, 1 * r"1e2")
  final object kilopascal extends DefaultPressureUnit("kilopascal", "kPa", Seq("KPa"), 1 * r"1e3")
  final object megapascal extends DefaultPressureUnit("megapascal", "MPa", Nil, 1 * r"1e6")
  final object gigapascal extends DefaultPressureUnit("gigapascal", "GPa", Nil, 1 * r"1e9")
  final object terapascal extends DefaultPressureUnit("terapascal", "TPa", Nil, 1 * r"1e12")
  final object petapascal extends DefaultPressureUnit("petapascal", "PPa", Nil, 1 * r"1e15")
  final object exapascal extends DefaultPressureUnit("exapascal", "EPa", Nil, 1 * r"1e18")
  final object zettapascal extends DefaultPressureUnit("zettapascal", "ZPa", Nil, 1 * r"1e21")
  final object yottapascal extends DefaultPressureUnit("yottapascal", "YPa", Nil, 1 * r"1e24")
  final object barye extends DefaultPressureUnit("barye", "Ba", Nil, ForceUnitObjects.dyne.interval / AreaUnitObjects.square_centimetre.interval)
  final object atmosphere extends DefaultPressureUnit("atmosphere", "atm", Nil, r"101325")
  final object atmosphere_technical extends DefaultPressureUnit("atmosphere technical", "at", Nil, ForceUnitObjects.kilogram_force.interval / AreaUnitObjects.square_centimetre.interval)
  final object bar extends DefaultPressureUnit("bar", "bar", Nil, r"1e5")
  final object pieze extends DefaultPressureUnit("pieze", "pz", Nil, r"1000")
  final object torr extends DefaultPressureUnit("torr", "torr", Nil, r"1"/r"760" * atmosphere.interval)
  final object kip_per_square_inch extends DefaultPressureUnit("kip per square_inch", "ksi", Nil, ForceUnitObjects.kip_force.interval / AreaUnitObjects.square_inch.interval)
  final object pound_per_square_foot extends DefaultPressureUnit("pound per square foot", "psf", Nil, ForceUnitObjects.pound_force.interval / AreaUnitObjects.square_foot.interval)
  final object pound_per_square_inch extends DefaultPressureUnit("pound per square inch", "psi", Nil, ForceUnitObjects.pound_force.interval / AreaUnitObjects.square_inch.interval)
  final object micrometre_of_mercury extends DefaultPressureUnit("micrometre of mercury", "μmHg", Seq("microMetreHg"), DensityUnitObjects.mercury.interval * LengthUnitObjects.micrometre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object millimetre_of_mercury extends DefaultPressureUnit("millimetre of mercury", "mmHg", Nil, DensityUnitObjects.mercury.interval * LengthUnitObjects.millimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object centimetre_of_mercury extends DefaultPressureUnit("centimetre of mercury", "cmHg", Nil, DensityUnitObjects.mercury.interval * LengthUnitObjects.centimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object inch_of_mercury extends DefaultPressureUnit("inch of mercury", "inHg", Nil, DensityUnitObjects.mercury.interval * LengthUnitObjects.inch.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object foot_of_mercury extends DefaultPressureUnit("foot of mercury", "ftHg", Nil, DensityUnitObjects.mercury.interval * LengthUnitObjects.foot.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object millimetre_of_water extends DefaultPressureUnit("millimetre of water", "mmH2O", Nil, DensityUnitObjects.water.interval * LengthUnitObjects.millimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object centimetre_of_water extends DefaultPressureUnit("centimetre of water", "cmH2O", Nil, DensityUnitObjects.water.interval * LengthUnitObjects.centimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object inch_of_water extends DefaultPressureUnit("inch of water", "inH2O", Nil, DensityUnitObjects.water.interval * LengthUnitObjects.inch.interval * AccelerationUnitObjects.standard_gravity.interval)
  final object foot_of_water extends DefaultPressureUnit("foot of water", "ftH2O", Nil, DensityUnitObjects.water.interval * LengthUnitObjects.foot.interval * AccelerationUnitObjects.standard_gravity.interval)
}

object PressureUnits{
  def Pa: PressureUnit = PressureUnitObjects.pascal
  def yPa: PressureUnit = PressureUnitObjects.yoctopascal
  def zPa: PressureUnit = PressureUnitObjects.zeptopascal
  def aPa: PressureUnit = PressureUnitObjects.attopascal
  def fPa: PressureUnit = PressureUnitObjects.femtopascal
  def pPa: PressureUnit = PressureUnitObjects.picopascal
  def nPa: PressureUnit = PressureUnitObjects.nanopascal
  def μPa: PressureUnit = PressureUnitObjects.micropascal
  def mcPa: PressureUnit = PressureUnitObjects.micropascal
  def mPa: PressureUnit = PressureUnitObjects.millipascal
  def cPa: PressureUnit = PressureUnitObjects.centipascal
  def dPa: PressureUnit = PressureUnitObjects.decipascal
  def daPa: PressureUnit = PressureUnitObjects.decapascal
  def hPa: PressureUnit = PressureUnitObjects.hectopascal
  def kPa: PressureUnit = PressureUnitObjects.kilopascal
  def KPa: PressureUnit = PressureUnitObjects.kilopascal
  def MPa: PressureUnit = PressureUnitObjects.megapascal
  def GPa: PressureUnit = PressureUnitObjects.gigapascal
  def TPa: PressureUnit = PressureUnitObjects.terapascal
  def PPa: PressureUnit = PressureUnitObjects.petapascal
  def EPa: PressureUnit = PressureUnitObjects.exapascal
  def ZPa: PressureUnit = PressureUnitObjects.zettapascal
  def YPa: PressureUnit = PressureUnitObjects.yottapascal
  def Ba: PressureUnit = PressureUnitObjects.barye
  def atm: PressureUnit = PressureUnitObjects.atmosphere
  def at: PressureUnit = PressureUnitObjects.atmosphere_technical
  def bar: PressureUnit = PressureUnitObjects.bar
  def pz: PressureUnit = PressureUnitObjects.pieze
  def torr: PressureUnit = PressureUnitObjects.torr
  def ksi: PressureUnit = PressureUnitObjects.kip_per_square_inch
  def psf: PressureUnit = PressureUnitObjects.pound_per_square_foot
  def psi: PressureUnit = PressureUnitObjects.pound_per_square_inch
  def μmHg: PressureUnit = PressureUnitObjects.micrometre_of_mercury
  def microMetreHg: PressureUnit = PressureUnitObjects.micrometre_of_mercury
  def mmHg: PressureUnit = PressureUnitObjects.millimetre_of_mercury
  def cmHg: PressureUnit = PressureUnitObjects.centimetre_of_mercury
  def inHg: PressureUnit = PressureUnitObjects.inch_of_mercury
  def ftHg: PressureUnit = PressureUnitObjects.foot_of_mercury
  def mmH2O: PressureUnit = PressureUnitObjects.millimetre_of_water
  def cmH2O: PressureUnit = PressureUnitObjects.centimetre_of_water
  def inH2O: PressureUnit = PressureUnitObjects.inch_of_water
  def ftH2O: PressureUnit = PressureUnitObjects.foot_of_water
}