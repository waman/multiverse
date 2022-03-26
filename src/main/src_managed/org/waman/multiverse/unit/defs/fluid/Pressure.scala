package org.waman.multiverse.unit.defs.fluid

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.mech._

class Pressure[A: Fractional](val value: A, val unit: PressureUnit)
    extends LinearQuantity[Pressure[A], A, PressureUnit] {

  override protected def newQuantity(value: A, unit: PressureUnit): Pressure[A] = new Pressure(value, unit)

  def *(time: Time[A]): DynamicViscosity[A] = new DynamicViscosity(this.value * time.value, this.unit * time.unit)

  def *(volume: Volume[A]): Energy[A] = new Energy(this.value * volume.value, volume.unit * this.unit)
}

/** None */
trait PressureUnit extends LinearUnit[PressureUnit]{

  override def getSIUnit: PressureUnit = PressureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = PressureUnit.dimension

  def *(timeUnit: TimeUnit): DynamicViscosityUnit =
    new ProductUnit[DynamicViscosityUnit, PressureUnit, TimeUnit](PressureUnit.this, timeUnit) with DynamicViscosityUnit
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


/** For no alias or user defined units */
class SimplePressureUnit(val name: String, val symbol: String, val interval: Real) extends PressureUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultPressureUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends PressureUnit
  
object PressureUnitObjects{

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
  final case object barye extends SimplePressureUnit("barye", "Ba", ForceUnitObjects.dyne.interval / AreaUnitObjects.square_centimetre.interval)
  final case object atmosphere extends SimplePressureUnit("atmosphere", "atm", r"101325")
  final case object atmosphere_technical extends SimplePressureUnit("atmosphere technical", "at", ForceUnitObjects.kilogram_force.interval / AreaUnitObjects.square_centimetre.interval)
  final case object bar extends SimplePressureUnit("bar", "bar", r"1e5")
  final case object pieze extends SimplePressureUnit("pieze", "pz", r"1000")
  final case object torr extends SimplePressureUnit("torr", "torr", r"1/760" * atmosphere.interval)
  final case object kip_per_square_inch extends SimplePressureUnit("kip per square inch", "ksi", ForceUnitObjects.kip_force.interval / AreaUnitObjects.square_inch.interval)
  final case object pound_per_square_foot extends SimplePressureUnit("pound per square foot", "psf", ForceUnitObjects.pound_force.interval / AreaUnitObjects.square_foot.interval)
  final case object pound_per_square_inch extends SimplePressureUnit("pound per square inch", "psi", ForceUnitObjects.pound_force.interval / AreaUnitObjects.square_inch.interval)
  final case object micrometre_of_mercury extends DefaultPressureUnit("micrometre of mercury", "μmHg", Seq("mcmHg"), DensityUnitObjects.mercury.interval * LengthUnitObjects.micrometre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object millimetre_of_mercury extends SimplePressureUnit("millimetre of mercury", "mmHg", DensityUnitObjects.mercury.interval * LengthUnitObjects.millimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object centimetre_of_mercury extends SimplePressureUnit("centimetre of mercury", "cmHg", DensityUnitObjects.mercury.interval * LengthUnitObjects.centimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object inch_of_mercury extends SimplePressureUnit("inch of mercury", "inHg", DensityUnitObjects.mercury.interval * LengthUnitObjects.inch.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object foot_of_mercury extends SimplePressureUnit("foot of mercury", "ftHg", DensityUnitObjects.mercury.interval * LengthUnitObjects.foot.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object millimetre_of_water extends DefaultPressureUnit("millimetre of water", "mmH₂O", Seq("mmH2O", "mmAq"), DensityUnitObjects.water.interval * LengthUnitObjects.millimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object centimetre_of_water extends DefaultPressureUnit("centimetre of water", "cmH₂O", Seq("cmH2O", "cmAq"), DensityUnitObjects.water.interval * LengthUnitObjects.centimetre.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object inch_of_water extends DefaultPressureUnit("inch of water", "inH₂O", Seq("inH2O"), DensityUnitObjects.water.interval * LengthUnitObjects.inch.interval * AccelerationUnitObjects.standard_gravity.interval)
  final case object foot_of_water extends DefaultPressureUnit("foot of water", "ftH₂O", Seq("ftH2O"), DensityUnitObjects.water.interval * LengthUnitObjects.foot.interval * AccelerationUnitObjects.standard_gravity.interval)
}


object PressureUnits{

  /** pascal */
  def Pa: PressureUnit = PressureUnitObjects.pascal
  /** yoctopascal */
  def yPa: PressureUnit = PressureUnitObjects.yoctopascal
  /** zeptopascal */
  def zPa: PressureUnit = PressureUnitObjects.zeptopascal
  /** attopascal */
  def aPa: PressureUnit = PressureUnitObjects.attopascal
  /** femtopascal */
  def fPa: PressureUnit = PressureUnitObjects.femtopascal
  /** picopascal */
  def pPa: PressureUnit = PressureUnitObjects.picopascal
  /** nanopascal */
  def nPa: PressureUnit = PressureUnitObjects.nanopascal
  /** micropascal */
  def μPa: PressureUnit = PressureUnitObjects.micropascal
  /** micropascal */
  def mcPa: PressureUnit = PressureUnitObjects.micropascal
  /** millipascal */
  def mPa: PressureUnit = PressureUnitObjects.millipascal
  /** centipascal */
  def cPa: PressureUnit = PressureUnitObjects.centipascal
  /** decipascal */
  def dPa: PressureUnit = PressureUnitObjects.decipascal
  /** decapascal */
  def daPa: PressureUnit = PressureUnitObjects.decapascal
  /** hectopascal */
  def hPa: PressureUnit = PressureUnitObjects.hectopascal
  /** kilopascal */
  def kPa: PressureUnit = PressureUnitObjects.kilopascal
  /** kilopascal */
  def KPa: PressureUnit = PressureUnitObjects.kilopascal
  /** megapascal */
  def MPa: PressureUnit = PressureUnitObjects.megapascal
  /** gigapascal */
  def GPa: PressureUnit = PressureUnitObjects.gigapascal
  /** terapascal */
  def TPa: PressureUnit = PressureUnitObjects.terapascal
  /** petapascal */
  def PPa: PressureUnit = PressureUnitObjects.petapascal
  /** exapascal */
  def EPa: PressureUnit = PressureUnitObjects.exapascal
  /** zettapascal */
  def ZPa: PressureUnit = PressureUnitObjects.zettapascal
  /** yottapascal */
  def YPa: PressureUnit = PressureUnitObjects.yottapascal
  /** barye */
  def Ba: PressureUnit = PressureUnitObjects.barye
  /** atmosphere */
  def atm: PressureUnit = PressureUnitObjects.atmosphere
  /** atmosphere technical */
  def at: PressureUnit = PressureUnitObjects.atmosphere_technical
  /** bar */
  def bar: PressureUnit = PressureUnitObjects.bar
  /** pieze */
  def pz: PressureUnit = PressureUnitObjects.pieze
  /** torr */
  def torr: PressureUnit = PressureUnitObjects.torr
  /** kip per square inch */
  def ksi: PressureUnit = PressureUnitObjects.kip_per_square_inch
  /** pound per square foot */
  def psf: PressureUnit = PressureUnitObjects.pound_per_square_foot
  /** pound per square inch */
  def psi: PressureUnit = PressureUnitObjects.pound_per_square_inch
  /** micrometre of mercury */
  def μmHg: PressureUnit = PressureUnitObjects.micrometre_of_mercury
  /** micrometre of mercury */
  def mcmHg: PressureUnit = PressureUnitObjects.micrometre_of_mercury
  /** millimetre of mercury */
  def mmHg: PressureUnit = PressureUnitObjects.millimetre_of_mercury
  /** centimetre of mercury */
  def cmHg: PressureUnit = PressureUnitObjects.centimetre_of_mercury
  /** inch of mercury */
  def inHg: PressureUnit = PressureUnitObjects.inch_of_mercury
  /** foot of mercury */
  def ftHg: PressureUnit = PressureUnitObjects.foot_of_mercury
  /** millimetre of water */
  def `mmH₂O`: PressureUnit = PressureUnitObjects.millimetre_of_water
  /** millimetre of water */
  def mmH2O: PressureUnit = PressureUnitObjects.millimetre_of_water
  /** millimetre of water */
  def mmAq: PressureUnit = PressureUnitObjects.millimetre_of_water
  /** centimetre of water */
  def `cmH₂O`: PressureUnit = PressureUnitObjects.centimetre_of_water
  /** centimetre of water */
  def cmH2O: PressureUnit = PressureUnitObjects.centimetre_of_water
  /** centimetre of water */
  def cmAq: PressureUnit = PressureUnitObjects.centimetre_of_water
  /** inch of water */
  def `inH₂O`: PressureUnit = PressureUnitObjects.inch_of_water
  /** inch of water */
  def inH2O: PressureUnit = PressureUnitObjects.inch_of_water
  /** foot of water */
  def `ftH₂O`: PressureUnit = PressureUnitObjects.foot_of_water
  /** foot of water */
  def ftH2O: PressureUnit = PressureUnitObjects.foot_of_water
}