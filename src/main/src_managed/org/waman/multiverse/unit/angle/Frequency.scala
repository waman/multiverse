package org.waman.multiverse.unit.angle

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Frequency[A: Fractional](val value: A, val unit: FrequencyUnit)
    extends LinearQuantity[Frequency[A], A, FrequencyUnit] {

  override protected def newQuantity(value: A, unit: FrequencyUnit): Frequency[A] = new Frequency(value, unit)

  import org.waman.multiverse.unit.Constants
  import FrequencyUnitObjects._
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  def toAngularVelocity: AngularVelocity[A] =
    new AngularVelocity(
      apply(heltz) * implicitly[Fractional[A]].fromReal(r"2" * Constants.Pi),
      AngleUnitObjects.radian / TimeUnitObjects.second)

}

trait FrequencyUnit extends LinearUnit[FrequencyUnit]{

  override def getSIUnit: FrequencyUnit = FrequencyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = FrequencyUnit.dimension

}

object FrequencyUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1).withDefaultValue(0)

  def getSIUnit: FrequencyUnit = FrequencyUnitObjects.heltz

  import FrequencyUnitObjects._
  def getUnits: Seq[FrequencyUnit] =
    Seq(heltz, yoctoheltz, zeptoheltz, attoheltz, femtoheltz, picoheltz, nanoheltz, microheltz, milliheltz, centiheltz, deciheltz, decaheltz, hectoheltz, kiloheltz, megaheltz, gigaheltz, teraheltz, petaheltz, exaheltz, zettaheltz, yottaheltz)
}

class DefaultFrequencyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends FrequencyUnit

object FrequencyUnitObjects{

  final object heltz extends DefaultFrequencyUnit("heltz", "Hz", Nil, 1)
  final object yoctoheltz extends DefaultFrequencyUnit("yoctoheltz", "yHz", Nil, 1 * r"1e-24")
  final object zeptoheltz extends DefaultFrequencyUnit("zeptoheltz", "zHz", Nil, 1 * r"1e-21")
  final object attoheltz extends DefaultFrequencyUnit("attoheltz", "aHz", Nil, 1 * r"1e-18")
  final object femtoheltz extends DefaultFrequencyUnit("femtoheltz", "fHz", Nil, 1 * r"1e-15")
  final object picoheltz extends DefaultFrequencyUnit("picoheltz", "pHz", Nil, 1 * r"1e-12")
  final object nanoheltz extends DefaultFrequencyUnit("nanoheltz", "nHz", Nil, 1 * r"1e-9")
  final object microheltz extends DefaultFrequencyUnit("microheltz", "μHz", Seq("mcHz"), 1 * r"1e-6")
  final object milliheltz extends DefaultFrequencyUnit("milliheltz", "mHz", Nil, 1 * r"1e-3")
  final object centiheltz extends DefaultFrequencyUnit("centiheltz", "cHz", Nil, 1 * r"1e-2")
  final object deciheltz extends DefaultFrequencyUnit("deciheltz", "dHz", Nil, 1 * r"1e-1")
  final object decaheltz extends DefaultFrequencyUnit("decaheltz", "daHz", Nil, 1 * r"1e1")
  final object hectoheltz extends DefaultFrequencyUnit("hectoheltz", "hHz", Nil, 1 * r"1e2")
  final object kiloheltz extends DefaultFrequencyUnit("kiloheltz", "kHz", Seq("KHz"), 1 * r"1e3")
  final object megaheltz extends DefaultFrequencyUnit("megaheltz", "MHz", Nil, 1 * r"1e6")
  final object gigaheltz extends DefaultFrequencyUnit("gigaheltz", "GHz", Nil, 1 * r"1e9")
  final object teraheltz extends DefaultFrequencyUnit("teraheltz", "THz", Nil, 1 * r"1e12")
  final object petaheltz extends DefaultFrequencyUnit("petaheltz", "PHz", Nil, 1 * r"1e15")
  final object exaheltz extends DefaultFrequencyUnit("exaheltz", "EHz", Nil, 1 * r"1e18")
  final object zettaheltz extends DefaultFrequencyUnit("zettaheltz", "ZHz", Nil, 1 * r"1e21")
  final object yottaheltz extends DefaultFrequencyUnit("yottaheltz", "YHz", Nil, 1 * r"1e24")
}

object FrequencyUnits{
  def Hz: FrequencyUnit = FrequencyUnitObjects.heltz
  def yHz: FrequencyUnit = FrequencyUnitObjects.yoctoheltz
  def zHz: FrequencyUnit = FrequencyUnitObjects.zeptoheltz
  def aHz: FrequencyUnit = FrequencyUnitObjects.attoheltz
  def fHz: FrequencyUnit = FrequencyUnitObjects.femtoheltz
  def pHz: FrequencyUnit = FrequencyUnitObjects.picoheltz
  def nHz: FrequencyUnit = FrequencyUnitObjects.nanoheltz
  def μHz: FrequencyUnit = FrequencyUnitObjects.microheltz
  def mcHz: FrequencyUnit = FrequencyUnitObjects.microheltz
  def mHz: FrequencyUnit = FrequencyUnitObjects.milliheltz
  def cHz: FrequencyUnit = FrequencyUnitObjects.centiheltz
  def dHz: FrequencyUnit = FrequencyUnitObjects.deciheltz
  def daHz: FrequencyUnit = FrequencyUnitObjects.decaheltz
  def hHz: FrequencyUnit = FrequencyUnitObjects.hectoheltz
  def kHz: FrequencyUnit = FrequencyUnitObjects.kiloheltz
  def KHz: FrequencyUnit = FrequencyUnitObjects.kiloheltz
  def MHz: FrequencyUnit = FrequencyUnitObjects.megaheltz
  def GHz: FrequencyUnit = FrequencyUnitObjects.gigaheltz
  def THz: FrequencyUnit = FrequencyUnitObjects.teraheltz
  def PHz: FrequencyUnit = FrequencyUnitObjects.petaheltz
  def EHz: FrequencyUnit = FrequencyUnitObjects.exaheltz
  def ZHz: FrequencyUnit = FrequencyUnitObjects.zettaheltz
  def YHz: FrequencyUnit = FrequencyUnitObjects.yottaheltz
}