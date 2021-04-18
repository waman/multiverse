package org.waman.multiverse.unit.defs.angle

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.radiometry._
import org.waman.multiverse.Constants

class Frequency[A: Fractional](val value: A, val unit: FrequencyUnit)
    extends LinearQuantity[Frequency[A], A, FrequencyUnit] {


  def toAngularVelocity: AngularVelocity[A] = new AngularVelocity(
      apply(FrequencyUnitObjects.heltz) * implicitly[Fractional[A]].fromReal(2 * Constants.Pi),
      AngleUnitObjects.radian / TimeUnitObjects.second)

  override protected def newQuantity(value: A, unit: FrequencyUnit): Frequency[A] = new Frequency(value, unit)

  def *(area: Area[A]): AreaFrequency[A] = new AreaFrequency(this.value * area.value, area.unit * this.unit)
}

/** None */
trait FrequencyUnit extends LinearUnit[FrequencyUnit]{

  override def getSIUnit: FrequencyUnit = FrequencyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = FrequencyUnit.dimension
}

object FrequencyUnit extends UnitInfo[FrequencyUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1).withDefaultValue(0)

  def getSIUnit: FrequencyUnit = FrequencyUnitObjects.heltz

  import FrequencyUnitObjects._

  def getUnits: Seq[FrequencyUnit] =
    Seq(heltz, yoctoheltz, zeptoheltz, attoheltz, femtoheltz, picoheltz, nanoheltz, microheltz, milliheltz, centiheltz, deciheltz, decaheltz, hectoheltz, kiloheltz, megaheltz, gigaheltz, teraheltz, petaheltz, exaheltz, zettaheltz, yottaheltz)
}


/** For no aliase or user defined units */
class SimpleFrequencyUnit(val name: String, val symbol: String, val interval: Real) extends FrequencyUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultFrequencyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends FrequencyUnit
  
object FrequencyUnitObjects{

  final case object heltz extends SimpleFrequencyUnit("heltz", "Hz", 1)
  final case object yoctoheltz extends SimpleFrequencyUnit("yoctoheltz", "yHz", r"1e-24")
  final case object zeptoheltz extends SimpleFrequencyUnit("zeptoheltz", "zHz", r"1e-21")
  final case object attoheltz extends SimpleFrequencyUnit("attoheltz", "aHz", r"1e-18")
  final case object femtoheltz extends SimpleFrequencyUnit("femtoheltz", "fHz", r"1e-15")
  final case object picoheltz extends SimpleFrequencyUnit("picoheltz", "pHz", r"1e-12")
  final case object nanoheltz extends SimpleFrequencyUnit("nanoheltz", "nHz", r"1e-9")
  final case object microheltz extends DefaultFrequencyUnit("microheltz", "μHz", Seq("mcHz"), r"1e-6")
  final case object milliheltz extends SimpleFrequencyUnit("milliheltz", "mHz", r"1e-3")
  final case object centiheltz extends SimpleFrequencyUnit("centiheltz", "cHz", r"1e-2")
  final case object deciheltz extends SimpleFrequencyUnit("deciheltz", "dHz", r"1e-1")
  final case object decaheltz extends SimpleFrequencyUnit("decaheltz", "daHz", r"1e1")
  final case object hectoheltz extends SimpleFrequencyUnit("hectoheltz", "hHz", r"1e2")
  final case object kiloheltz extends DefaultFrequencyUnit("kiloheltz", "kHz", Seq("KHz"), r"1e3")
  final case object megaheltz extends SimpleFrequencyUnit("megaheltz", "MHz", r"1e6")
  final case object gigaheltz extends SimpleFrequencyUnit("gigaheltz", "GHz", r"1e9")
  final case object teraheltz extends SimpleFrequencyUnit("teraheltz", "THz", r"1e12")
  final case object petaheltz extends SimpleFrequencyUnit("petaheltz", "PHz", r"1e15")
  final case object exaheltz extends SimpleFrequencyUnit("exaheltz", "EHz", r"1e18")
  final case object zettaheltz extends SimpleFrequencyUnit("zettaheltz", "ZHz", r"1e21")
  final case object yottaheltz extends SimpleFrequencyUnit("yottaheltz", "YHz", r"1e24")
}


object FrequencyUnits{

  /** heltz */
  def Hz: FrequencyUnit = FrequencyUnitObjects.heltz
  /** yoctoheltz */
  def yHz: FrequencyUnit = FrequencyUnitObjects.yoctoheltz
  /** zeptoheltz */
  def zHz: FrequencyUnit = FrequencyUnitObjects.zeptoheltz
  /** attoheltz */
  def aHz: FrequencyUnit = FrequencyUnitObjects.attoheltz
  /** femtoheltz */
  def fHz: FrequencyUnit = FrequencyUnitObjects.femtoheltz
  /** picoheltz */
  def pHz: FrequencyUnit = FrequencyUnitObjects.picoheltz
  /** nanoheltz */
  def nHz: FrequencyUnit = FrequencyUnitObjects.nanoheltz
  /** microheltz */
  def μHz: FrequencyUnit = FrequencyUnitObjects.microheltz
  /** microheltz */
  def mcHz: FrequencyUnit = FrequencyUnitObjects.microheltz
  /** milliheltz */
  def mHz: FrequencyUnit = FrequencyUnitObjects.milliheltz
  /** centiheltz */
  def cHz: FrequencyUnit = FrequencyUnitObjects.centiheltz
  /** deciheltz */
  def dHz: FrequencyUnit = FrequencyUnitObjects.deciheltz
  /** decaheltz */
  def daHz: FrequencyUnit = FrequencyUnitObjects.decaheltz
  /** hectoheltz */
  def hHz: FrequencyUnit = FrequencyUnitObjects.hectoheltz
  /** kiloheltz */
  def kHz: FrequencyUnit = FrequencyUnitObjects.kiloheltz
  /** kiloheltz */
  def KHz: FrequencyUnit = FrequencyUnitObjects.kiloheltz
  /** megaheltz */
  def MHz: FrequencyUnit = FrequencyUnitObjects.megaheltz
  /** gigaheltz */
  def GHz: FrequencyUnit = FrequencyUnitObjects.gigaheltz
  /** teraheltz */
  def THz: FrequencyUnit = FrequencyUnitObjects.teraheltz
  /** petaheltz */
  def PHz: FrequencyUnit = FrequencyUnitObjects.petaheltz
  /** exaheltz */
  def EHz: FrequencyUnit = FrequencyUnitObjects.exaheltz
  /** zettaheltz */
  def ZHz: FrequencyUnit = FrequencyUnitObjects.zettaheltz
  /** yottaheltz */
  def YHz: FrequencyUnit = FrequencyUnitObjects.yottaheltz
}