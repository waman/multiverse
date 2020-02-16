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

/** null */
trait PressureUnit extends LinearUnit[PressureUnit]{

  override def getSIUnit: PressureUnit = PressureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = PressureUnit.dimension

  def *(timeUnit: TimeUnit): DynamicViscosityUnit =
    new AbstractProductUnit[DynamicViscosityUnit, PressureUnit, TimeUnit](PressureUnit.this, timeUnit) with DynamicViscosityUnit
}

object PressureUnit extends UnitInfo[PressureUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> -1).withDefaultValue(0)

  def getSIUnit: PressureUnit = PressureUnitObjects.pascal

  import PressureUnitObjects._
  def getUnits: Seq[PressureUnit] =
    Seq(pascal, yoctopascal, zeptopascal, attopascal, femtopascal, picopascal, nanopascal, micropascal, millipascal, centipascal, decipascal, decapascal, hectopascal, kilopascal, megapascal, gigapascal, terapascal, petapascal, exapascal, zettapascal, yottapascal, barye, atmosphere, atmosphere_technical, bar, pieze, torr, kip_per_square_inch, pound_per_square_foot, pound_per_square_inch, micrometre_of_mercury, millimetre_of_mercury, centimetre_of_mercury, inch_of_mercury, foot_of_mercury, millimetre_of_water, centimetre_of_water, inch_of_water, foot_of_water)
}

/** For no aliase or user defined units */
class SimplePressureUnit(val name: String, val symbol: String, val interval: Real) extends PressureUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultPressureUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends PressureUnit

object PressureUnitObjects{
  import org.waman.multiverse.unit.mechanics.ForceUnitObjects._
  import org.waman.multiverse.unit.basic.AreaUnitObjects._
  import org.waman.multiverse.unit.density.DensityUnitObjects._
  import org.waman.multiverse.unit.basic.LengthUnitObjects._
  import org.waman.multiverse.unit.mechanics.AccelerationUnitObjects._

  final case object pascal extends SimplePressureUnit("pascal", "Pa", 1)
  final case object yoctopascal extends SimplePressureUnit("yoctopascal", "yPa", r"1e-24")
  final case object zeptopascal extends SimplePressureUnit("zeptopascal", "zPa", r"1e-21")
  final case object attopascal extends SimplePressureUnit("attopascal", "aPa", r"1e-18")
  final case object femtopascal extends SimplePressureUnit("femtopascal", "fPa", r"1e-15")
  final case object picopascal extends SimplePressureUnit("picopascal", "pPa", r"1e-12")
  final case object nanopascal extends SimplePressureUnit("nanopascal", "nPa", r"1e-9")
  final case object micropascal extends DefaultPressureUnit("micropascal", "μPa", Seq("mcPa"), r"1e-6")
  final case object millipascal extends SimplePressureUnit("millipascal", "mPa", r"1e-3")
  final case object centipascal extends SimplePressureUnit("centipascal", "cPa", r"1e-2")
  final case object decipascal extends SimplePressureUnit("decipascal", "dPa", r"1e-1")
  final case object decapascal extends SimplePressureUnit("decapascal", "daPa", r"1e1")
  final case object hectopascal extends SimplePressureUnit("hectopascal", "hPa", r"1e2")
  final case object kilopascal extends DefaultPressureUnit("kilopascal", "kPa", Seq("KPa"), r"1e3")
  final case object megapascal extends SimplePressureUnit("megapascal", "MPa", r"1e6")
  final case object gigapascal extends SimplePressureUnit("gigapascal", "GPa", r"1e9")
  final case object terapascal extends SimplePressureUnit("terapascal", "TPa", r"1e12")
  final case object petapascal extends SimplePressureUnit("petapascal", "PPa", r"1e15")
  final case object exapascal extends SimplePressureUnit("exapascal", "EPa", r"1e18")
  final case object zettapascal extends SimplePressureUnit("zettapascal", "ZPa", r"1e21")
  final case object yottapascal extends SimplePressureUnit("yottapascal", "YPa", r"1e24")
  final case object barye extends SimplePressureUnit("barye", "Ba", dyne.interval / square_centimetre.interval)
  final case object atmosphere extends SimplePressureUnit("atmosphere", "atm", r"101325")
  final case object atmosphere_technical extends SimplePressureUnit("atmosphere technical", "at", kilogram_force.interval / square_centimetre.interval)
  final case object bar extends SimplePressureUnit("bar", "bar", r"1e5")
  final case object pieze extends SimplePressureUnit("pieze", "pz", r"1000")
  final case object torr extends SimplePressureUnit("torr", "torr", r"1"/r"760" * atmosphere.interval)
  final case object kip_per_square_inch extends SimplePressureUnit("kip per square_inch", "ksi", kip_force.interval / square_inch.interval)
  final case object pound_per_square_foot extends SimplePressureUnit("pound per square foot", "psf", pound_force.interval / square_foot.interval)
  final case object pound_per_square_inch extends SimplePressureUnit("pound per square inch", "psi", pound_force.interval / square_inch.interval)
  final case object micrometre_of_mercury extends DefaultPressureUnit("micrometre of mercury", "μmHg", Seq("mcmHg"), mercury.interval * micrometre.interval * standard_gravity.interval)
  final case object millimetre_of_mercury extends SimplePressureUnit("millimetre of mercury", "mmHg", mercury.interval * millimetre.interval * standard_gravity.interval)
  final case object centimetre_of_mercury extends SimplePressureUnit("centimetre of mercury", "cmHg", mercury.interval * centimetre.interval * standard_gravity.interval)
  final case object inch_of_mercury extends SimplePressureUnit("inch of mercury", "inHg", mercury.interval * inch.interval * standard_gravity.interval)
  final case object foot_of_mercury extends SimplePressureUnit("foot of mercury", "ftHg", mercury.interval * foot.interval * standard_gravity.interval)
  final case object millimetre_of_water extends DefaultPressureUnit("millimetre of water", "mmH₂O", Seq("mmH2O", "mmAq"), water.interval * millimetre.interval * standard_gravity.interval)
  final case object centimetre_of_water extends DefaultPressureUnit("centimetre of water", "cmH₂O", Seq("cmH2O", "cmAq"), water.interval * centimetre.interval * standard_gravity.interval)
  final case object inch_of_water extends DefaultPressureUnit("inch of water", "inH₂O", Seq("inH2O"), water.interval * inch.interval * standard_gravity.interval)
  final case object foot_of_water extends DefaultPressureUnit("foot of water", "ftH₂O", Seq("ftH2O"), water.interval * foot.interval * standard_gravity.interval)
}

object PressureUnits{
  def Pa: PressureUnit = PressureUnitObjects.pascal
  def yPa: PressureUnit = PressureUnitObjects.yoctopascal
  def zPa: PressureUnit = PressureUnitObjects.zeptopascal
  def aPa: PressureUnit = PressureUnitObjects.attopascal
  def fPa: PressureUnit = PressureUnitObjects.femtopascal
  def pPa: PressureUnit = PressureUnitObjects.picopascal
  def nPa: PressureUnit = PressureUnitObjects.nanopascal
  def `μPa`: PressureUnit = PressureUnitObjects.micropascal
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
  def `μmHg`: PressureUnit = PressureUnitObjects.micrometre_of_mercury
  def mcmHg: PressureUnit = PressureUnitObjects.micrometre_of_mercury
  def mmHg: PressureUnit = PressureUnitObjects.millimetre_of_mercury
  def cmHg: PressureUnit = PressureUnitObjects.centimetre_of_mercury
  def inHg: PressureUnit = PressureUnitObjects.inch_of_mercury
  def ftHg: PressureUnit = PressureUnitObjects.foot_of_mercury
  def `mmH₂O`: PressureUnit = PressureUnitObjects.millimetre_of_water
  def mmH2O: PressureUnit = PressureUnitObjects.millimetre_of_water
  def mmAq: PressureUnit = PressureUnitObjects.millimetre_of_water
  def `cmH₂O`: PressureUnit = PressureUnitObjects.centimetre_of_water
  def cmH2O: PressureUnit = PressureUnitObjects.centimetre_of_water
  def cmAq: PressureUnit = PressureUnitObjects.centimetre_of_water
  def `inH₂O`: PressureUnit = PressureUnitObjects.inch_of_water
  def inH2O: PressureUnit = PressureUnitObjects.inch_of_water
  def `ftH₂O`: PressureUnit = PressureUnitObjects.foot_of_water
  def ftH2O: PressureUnit = PressureUnitObjects.foot_of_water
}