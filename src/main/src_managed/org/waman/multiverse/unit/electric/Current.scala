package org.waman.multiverse.unit.electric

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit

class Current[A: Fractional](val value: A, val unit: CurrentUnit)
    extends LinearQuantity[Current[A], A, CurrentUnit] {

  override protected def newQuantity(value: A, unit: CurrentUnit): Current[A] = new Current(value, unit)
           
  def *(time: Time[A]): Charge[A] = new Charge(this.value * time.value, this.unit * time.unit)

}

trait CurrentUnit extends LinearUnit[CurrentUnit]{
  override def getSIUnit: CurrentUnit = CurrentUnitObjects.getSIUnit


  def *(timeUnit: TimeUnit): ChargeUnit =
    new ProductUnit[ChargeUnit, CurrentUnit, TimeUnit](CurrentUnit.this, timeUnit) with ChargeUnit
}

class DefaultCurrentUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends CurrentUnit


object CurrentUnitObjects{

  def getSIUnit: CurrentUnit = ampere

  final object ampere extends DefaultCurrentUnit("ampere", "A", Nil, r"1")
  final object yoctoampere extends DefaultCurrentUnit("yoctoampere", "yA", Nil, r"1" * r"1e-24")
  final object zeptoampere extends DefaultCurrentUnit("zeptoampere", "zA", Nil, r"1" * r"1e-21")
  final object attoampere extends DefaultCurrentUnit("attoampere", "aA", Nil, r"1" * r"1e-18")
  final object femtoampere extends DefaultCurrentUnit("femtoampere", "fA", Nil, r"1" * r"1e-15")
  final object picoampere extends DefaultCurrentUnit("picoampere", "pA", Nil, r"1" * r"1e-12")
  final object nanoampere extends DefaultCurrentUnit("nanoampere", "nA", Nil, r"1" * r"1e-9")
  final object microampere extends DefaultCurrentUnit("microampere", "μA", Seq("mcA"), r"1" * r"1e-6")
  final object milliampere extends DefaultCurrentUnit("milliampere", "mA", Nil, r"1" * r"1e-3")
  final object centiampere extends DefaultCurrentUnit("centiampere", "cA", Nil, r"1" * r"1e-2")
  final object deciampere extends DefaultCurrentUnit("deciampere", "dA", Nil, r"1" * r"1e-1")
  final object decaampere extends DefaultCurrentUnit("decaampere", "daA", Nil, r"1" * r"1e1")
  final object hectoampere extends DefaultCurrentUnit("hectoampere", "hA", Nil, r"1" * r"1e2")
  final object kiloampere extends DefaultCurrentUnit("kiloampere", "kA", Seq("KA"), r"1" * r"1e3")
  final object megaampere extends DefaultCurrentUnit("megaampere", "MA", Nil, r"1" * r"1e6")
  final object gigaampere extends DefaultCurrentUnit("gigaampere", "GA", Nil, r"1" * r"1e9")
  final object teraampere extends DefaultCurrentUnit("teraampere", "TA", Nil, r"1" * r"1e12")
  final object petaampere extends DefaultCurrentUnit("petaampere", "PA", Nil, r"1" * r"1e15")
  final object exaampere extends DefaultCurrentUnit("exaampere", "EA", Nil, r"1" * r"1e18")
  final object zettaampere extends DefaultCurrentUnit("zettaampere", "ZA", Nil, r"1" * r"1e21")
  final object yottaampere extends DefaultCurrentUnit("yottaampere", "YA", Nil, r"1" * r"1e24")
  final object abampere extends DefaultCurrentUnit("abampere", "abamp", Nil, r"10")

  def getUnits: Seq[CurrentUnit] =
    Seq(ampere, yoctoampere, zeptoampere, attoampere, femtoampere, picoampere, nanoampere, microampere, milliampere, centiampere, deciampere, decaampere, hectoampere, kiloampere, megaampere, gigaampere, teraampere, petaampere, exaampere, zettaampere, yottaampere, abampere)
}


object CurrentUnits{
  def A: CurrentUnit = CurrentUnitObjects.ampere
  def yA: CurrentUnit = CurrentUnitObjects.yoctoampere
  def zA: CurrentUnit = CurrentUnitObjects.zeptoampere
  def aA: CurrentUnit = CurrentUnitObjects.attoampere
  def fA: CurrentUnit = CurrentUnitObjects.femtoampere
  def pA: CurrentUnit = CurrentUnitObjects.picoampere
  def nA: CurrentUnit = CurrentUnitObjects.nanoampere
  def μA: CurrentUnit = CurrentUnitObjects.microampere
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

  def getSIUnit: CurrentUnit = CurrentUnitObjects.getSIUnit
  def getUnits: Seq[CurrentUnit] = CurrentUnitObjects.getUnits
}
