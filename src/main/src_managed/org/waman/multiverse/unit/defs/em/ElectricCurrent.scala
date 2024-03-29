package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.Constants

class ElectricCurrent[A: Fractional](val value: A, val unit: ElectricCurrentUnit)
    extends LinearQuantity[ElectricCurrent[A], A, ElectricCurrentUnit] {

  override protected def newQuantity(value: A, unit: ElectricCurrentUnit): ElectricCurrent[A] = new ElectricCurrent(value, unit)

  def /(voltage: Voltage[A]): ElectricalConductance[A] = new ElectricalConductance(this.value / voltage.value, this.unit / voltage.unit)

  def *(time: Time[A]): ElectricCharge[A] = new ElectricCharge(this.value * time.value, this.unit * time.unit)

  def /(length: Length[A]): MagneticFieldStrength[A] = new MagneticFieldStrength(this.value / length.value, this.unit / length.unit)
}

/** None */
trait ElectricCurrentUnit extends LinearUnit[ElectricCurrentUnit]{

  override def getSIUnit: ElectricCurrentUnit = ElectricCurrentUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ElectricCurrentUnit.dimension

  def /(voltageUnit: VoltageUnit): ElectricalConductanceUnit =
    new QuotientUnit[ElectricalConductanceUnit, ElectricCurrentUnit, VoltageUnit](ElectricCurrentUnit.this, voltageUnit) with ElectricalConductanceUnit

  def *(timeUnit: TimeUnit): ElectricChargeUnit =
    new ProductUnit[ElectricChargeUnit, ElectricCurrentUnit, TimeUnit](ElectricCurrentUnit.this, timeUnit) with ElectricChargeUnit

  def /(lengthUnit: LengthUnit): MagneticFieldStrengthUnit =
    new QuotientUnit[MagneticFieldStrengthUnit, ElectricCurrentUnit, LengthUnit](ElectricCurrentUnit.this, lengthUnit) with MagneticFieldStrengthUnit
}

object ElectricCurrentUnit extends UnitInfo[ElectricCurrentUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](I -> 1).withDefaultValue(0)

  def getSIUnit: ElectricCurrentUnit = ElectricCurrentUnitObjects.ampere

  import ElectricCurrentUnitObjects._

  def getUnits: Seq[ElectricCurrentUnit] =
    Seq(ampere, yoctoampere, zeptoampere, attoampere, femtoampere, picoampere, nanoampere, microampere, milliampere, centiampere, deciampere, decaampere, hectoampere, kiloampere, megaampere, gigaampere, teraampere, petaampere, exaampere, zettaampere, yottaampere, abampere, statampere)
}


/** For no alias or user defined units */
class SimpleElectricCurrentUnit(val name: String, val symbol: String, val interval: Real) extends ElectricCurrentUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultElectricCurrentUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ElectricCurrentUnit
  
object ElectricCurrentUnitObjects{

  final case object ampere extends SimpleElectricCurrentUnit("ampere", "A", 1)
  final case object yoctoampere extends SimpleElectricCurrentUnit("yoctoampere", "yA", r"1e-24")
  final case object zeptoampere extends SimpleElectricCurrentUnit("zeptoampere", "zA", r"1e-21")
  final case object attoampere extends SimpleElectricCurrentUnit("attoampere", "aA", r"1e-18")
  final case object femtoampere extends SimpleElectricCurrentUnit("femtoampere", "fA", r"1e-15")
  final case object picoampere extends SimpleElectricCurrentUnit("picoampere", "pA", r"1e-12")
  final case object nanoampere extends SimpleElectricCurrentUnit("nanoampere", "nA", r"1e-9")
  final case object microampere extends DefaultElectricCurrentUnit("microampere", "μA", Seq("mcA"), r"1e-6")
  final case object milliampere extends SimpleElectricCurrentUnit("milliampere", "mA", r"1e-3")
  final case object centiampere extends SimpleElectricCurrentUnit("centiampere", "cA", r"1e-2")
  final case object deciampere extends SimpleElectricCurrentUnit("deciampere", "dA", r"1e-1")
  final case object decaampere extends SimpleElectricCurrentUnit("decaampere", "daA", r"1e1")
  final case object hectoampere extends SimpleElectricCurrentUnit("hectoampere", "hA", r"1e2")
  final case object kiloampere extends DefaultElectricCurrentUnit("kiloampere", "kA", Seq("KA"), r"1e3")
  final case object megaampere extends SimpleElectricCurrentUnit("megaampere", "MA", r"1e6")
  final case object gigaampere extends SimpleElectricCurrentUnit("gigaampere", "GA", r"1e9")
  final case object teraampere extends SimpleElectricCurrentUnit("teraampere", "TA", r"1e12")
  final case object petaampere extends SimpleElectricCurrentUnit("petaampere", "PA", r"1e15")
  final case object exaampere extends SimpleElectricCurrentUnit("exaampere", "EA", r"1e18")
  final case object zettaampere extends SimpleElectricCurrentUnit("zettaampere", "ZA", r"1e21")
  final case object yottaampere extends SimpleElectricCurrentUnit("yottaampere", "YA", r"1e24")
  final case object abampere extends DefaultElectricCurrentUnit("abampere", "abA", Seq("abamp", "Bi"), r"10")
  final case object statampere extends SimpleElectricCurrentUnit("statampere", "statA", r"0.1" / Constants.SpeedOfLight)
}


object ElectricCurrentUnits{

  /** ampere */
  def A: ElectricCurrentUnit = ElectricCurrentUnitObjects.ampere
  /** yoctoampere */
  def yA: ElectricCurrentUnit = ElectricCurrentUnitObjects.yoctoampere
  /** zeptoampere */
  def zA: ElectricCurrentUnit = ElectricCurrentUnitObjects.zeptoampere
  /** attoampere */
  def aA: ElectricCurrentUnit = ElectricCurrentUnitObjects.attoampere
  /** femtoampere */
  def fA: ElectricCurrentUnit = ElectricCurrentUnitObjects.femtoampere
  /** picoampere */
  def pA: ElectricCurrentUnit = ElectricCurrentUnitObjects.picoampere
  /** nanoampere */
  def nA: ElectricCurrentUnit = ElectricCurrentUnitObjects.nanoampere
  /** microampere */
  def μA: ElectricCurrentUnit = ElectricCurrentUnitObjects.microampere
  /** microampere */
  def mcA: ElectricCurrentUnit = ElectricCurrentUnitObjects.microampere
  /** milliampere */
  def mA: ElectricCurrentUnit = ElectricCurrentUnitObjects.milliampere
  /** centiampere */
  def cA: ElectricCurrentUnit = ElectricCurrentUnitObjects.centiampere
  /** deciampere */
  def dA: ElectricCurrentUnit = ElectricCurrentUnitObjects.deciampere
  /** decaampere */
  def daA: ElectricCurrentUnit = ElectricCurrentUnitObjects.decaampere
  /** hectoampere */
  def hA: ElectricCurrentUnit = ElectricCurrentUnitObjects.hectoampere
  /** kiloampere */
  def kA: ElectricCurrentUnit = ElectricCurrentUnitObjects.kiloampere
  /** kiloampere */
  def KA: ElectricCurrentUnit = ElectricCurrentUnitObjects.kiloampere
  /** megaampere */
  def MA: ElectricCurrentUnit = ElectricCurrentUnitObjects.megaampere
  /** gigaampere */
  def GA: ElectricCurrentUnit = ElectricCurrentUnitObjects.gigaampere
  /** teraampere */
  def TA: ElectricCurrentUnit = ElectricCurrentUnitObjects.teraampere
  /** petaampere */
  def PA: ElectricCurrentUnit = ElectricCurrentUnitObjects.petaampere
  /** exaampere */
  def EA: ElectricCurrentUnit = ElectricCurrentUnitObjects.exaampere
  /** zettaampere */
  def ZA: ElectricCurrentUnit = ElectricCurrentUnitObjects.zettaampere
  /** yottaampere */
  def YA: ElectricCurrentUnit = ElectricCurrentUnitObjects.yottaampere
  /** abampere */
  def abA: ElectricCurrentUnit = ElectricCurrentUnitObjects.abampere
  /** abampere */
  def abamp: ElectricCurrentUnit = ElectricCurrentUnitObjects.abampere
  /** abampere */
  def Bi: ElectricCurrentUnit = ElectricCurrentUnitObjects.abampere
  /** statampere */
  def statA: ElectricCurrentUnit = ElectricCurrentUnitObjects.statampere
}