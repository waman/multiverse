package waman.multiverse.unit.electrics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


import waman.multiverse.unit.basic.Time
import waman.multiverse.unit.basic.TimeUnit


class Current[A: Fractional](val value: A, val unit: CurrentUnit)
    extends LinearQuantity[Current[A], A, CurrentUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: CurrentUnit): Current[A] = new Current(value, unit)

  def *(time: Time[A]): Charge[A] = new Charge(this.value * time.value, this.unit * time.unit)

  def /(voltage: Voltage[A]): Conductance[A] = new Conductance(this.value / voltage.value, this.unit / voltage.unit)
}

trait CurrentUnit extends LinearUnit[CurrentUnit]{

  override def getSIUnit: CurrentUnit = CurrentUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = CurrentUnit.dimension

  def *(timeUnit: TimeUnit): ChargeUnit =
    new ProductUnit[ChargeUnit, CurrentUnit, TimeUnit](CurrentUnit.this, timeUnit) with ChargeUnit

  def /(voltageUnit: VoltageUnit): ConductanceUnit =
    new QuotientUnit[ConductanceUnit, CurrentUnit, VoltageUnit](CurrentUnit.this, voltageUnit) with ConductanceUnit
}

object CurrentUnit extends UnitInfo[CurrentUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](I -> 1).withDefaultValue(0)

  def getSIUnit: CurrentUnit = CurrentUnitObjects.ampere

  import CurrentUnitObjects._
  def getUnits: Seq[CurrentUnit] =
    Seq(ampere, yoctoampere, zeptoampere, attoampere, femtoampere, picoampere, nanoampere, microampere, milliampere, centiampere, deciampere, decaampere, hectoampere, kiloampere, megaampere, gigaampere, teraampere, petaampere, exaampere, zettaampere, yottaampere, abampere)
}

/** For no aliase or user defined units */
class SimpleCurrentUnit(val name: String, val symbol: String, val interval: Real) extends CurrentUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultCurrentUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends CurrentUnit

object CurrentUnitObjects{

  import spire.implicits._


  final case object ampere extends SimpleCurrentUnit("ampere", "A", 1)
  final case object yoctoampere extends SimpleCurrentUnit("yoctoampere", "yA", r"1e-24")
  final case object zeptoampere extends SimpleCurrentUnit("zeptoampere", "zA", r"1e-21")
  final case object attoampere extends SimpleCurrentUnit("attoampere", "aA", r"1e-18")
  final case object femtoampere extends SimpleCurrentUnit("femtoampere", "fA", r"1e-15")
  final case object picoampere extends SimpleCurrentUnit("picoampere", "pA", r"1e-12")
  final case object nanoampere extends SimpleCurrentUnit("nanoampere", "nA", r"1e-9")
  final case object microampere extends DefaultCurrentUnit("microampere", "μA", Seq("mcA"), r"1e-6")
  final case object milliampere extends SimpleCurrentUnit("milliampere", "mA", r"1e-3")
  final case object centiampere extends SimpleCurrentUnit("centiampere", "cA", r"1e-2")
  final case object deciampere extends SimpleCurrentUnit("deciampere", "dA", r"1e-1")
  final case object decaampere extends SimpleCurrentUnit("decaampere", "daA", r"1e1")
  final case object hectoampere extends SimpleCurrentUnit("hectoampere", "hA", r"1e2")
  final case object kiloampere extends DefaultCurrentUnit("kiloampere", "kA", Seq("KA"), r"1e3")
  final case object megaampere extends SimpleCurrentUnit("megaampere", "MA", r"1e6")
  final case object gigaampere extends SimpleCurrentUnit("gigaampere", "GA", r"1e9")
  final case object teraampere extends SimpleCurrentUnit("teraampere", "TA", r"1e12")
  final case object petaampere extends SimpleCurrentUnit("petaampere", "PA", r"1e15")
  final case object exaampere extends SimpleCurrentUnit("exaampere", "EA", r"1e18")
  final case object zettaampere extends SimpleCurrentUnit("zettaampere", "ZA", r"1e21")
  final case object yottaampere extends SimpleCurrentUnit("yottaampere", "YA", r"1e24")
  final case object abampere extends SimpleCurrentUnit("abampere", "abamp", r"10")
}

object CurrentUnits{

  def A: CurrentUnit = CurrentUnitObjects.ampere
  def yA: CurrentUnit = CurrentUnitObjects.yoctoampere
  def zA: CurrentUnit = CurrentUnitObjects.zeptoampere
  def aA: CurrentUnit = CurrentUnitObjects.attoampere
  def fA: CurrentUnit = CurrentUnitObjects.femtoampere
  def pA: CurrentUnit = CurrentUnitObjects.picoampere
  def nA: CurrentUnit = CurrentUnitObjects.nanoampere
  def `μA`: CurrentUnit = CurrentUnitObjects.microampere
  def mcA: CurrentUnit = CurrentUnitObjects.microampere
  def mA: CurrentUnit = CurrentUnitObjects.milliampere
  def cA: CurrentUnit = CurrentUnitObjects.centiampere
  def dA: CurrentUnit = CurrentUnitObjects.deciampere
  def daA: CurrentUnit = CurrentUnitObjects.decaampere
  def hA: CurrentUnit = CurrentUnitObjects.hectoampere
  def kA: CurrentUnit = CurrentUnitObjects.kiloampere
  def KA: CurrentUnit = CurrentUnitObjects.kiloampere
  def MA: CurrentUnit = CurrentUnitObjects.megaampere
  def GA: CurrentUnit = CurrentUnitObjects.gigaampere
  def TA: CurrentUnit = CurrentUnitObjects.teraampere
  def PA: CurrentUnit = CurrentUnitObjects.petaampere
  def EA: CurrentUnit = CurrentUnitObjects.exaampere
  def ZA: CurrentUnit = CurrentUnitObjects.zettaampere
  def YA: CurrentUnit = CurrentUnitObjects.yottaampere
  def abamp: CurrentUnit = CurrentUnitObjects.abampere
}