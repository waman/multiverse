package org.waman.multiverse.unit.electric

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class Charge[A: Fractional](val value: A, val unit: ChargeUnit)
    extends LinearQuantity[Charge[A], A, ChargeUnit] {

  override protected def newQuantity(value: A, unit: ChargeUnit): Charge[A] = new Charge(value, unit)
}

trait ChargeUnit extends LinearUnit[ChargeUnit]{
  override def getSIUnit: ChargeUnit = ChargeUnitObjects.getSIUnit

  import org.waman.multiverse.unit.basic.LengthUnit

  def *(lengthUnit: LengthUnit): DipoleUnit =
    new ProductUnit[DipoleUnit, ChargeUnit, LengthUnit](ChargeUnit.this, lengthUnit) with DipoleUnit

}

class DefaultChargeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ChargeUnit


object ChargeUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object coulomb extends DefaultChargeUnit("coulomb", "C", Nil, r"1")
  final object yoctocoulomb extends DefaultChargeUnit("yoctocoulomb", "yC", Nil, r"1" * r"1e-24")
  final object zeptocoulomb extends DefaultChargeUnit("zeptocoulomb", "zC", Nil, r"1" * r"1e-21")
  final object attocoulomb extends DefaultChargeUnit("attocoulomb", "aC", Nil, r"1" * r"1e-18")
  final object femtocoulomb extends DefaultChargeUnit("femtocoulomb", "fC", Nil, r"1" * r"1e-15")
  final object picocoulomb extends DefaultChargeUnit("picocoulomb", "pC", Nil, r"1" * r"1e-12")
  final object nanocoulomb extends DefaultChargeUnit("nanocoulomb", "nC", Nil, r"1" * r"1e-9")
  final object microcoulomb extends DefaultChargeUnit("microcoulomb", "μC", Seq("mcC"), r"1" * r"1e-6")
  final object millicoulomb extends DefaultChargeUnit("millicoulomb", "mC", Nil, r"1" * r"1e-3")
  final object centicoulomb extends DefaultChargeUnit("centicoulomb", "cC", Nil, r"1" * r"1e-2")
  final object decicoulomb extends DefaultChargeUnit("decicoulomb", "dC", Nil, r"1" * r"1e-1")
  final object decacoulomb extends DefaultChargeUnit("decacoulomb", "daC", Nil, r"1" * r"1e1")
  final object hectocoulomb extends DefaultChargeUnit("hectocoulomb", "hC", Nil, r"1" * r"1e2")
  final object kilocoulomb extends DefaultChargeUnit("kilocoulomb", "kC", Nil, r"1" * r"1e3")
  final object megacoulomb extends DefaultChargeUnit("megacoulomb", "MC", Nil, r"1" * r"1e6")
  final object gigacoulomb extends DefaultChargeUnit("gigacoulomb", "GC", Nil, r"1" * r"1e9")
  final object teracoulomb extends DefaultChargeUnit("teracoulomb", "TC", Nil, r"1" * r"1e12")
  final object petacoulomb extends DefaultChargeUnit("petacoulomb", "PC", Nil, r"1" * r"1e15")
  final object exacoulomb extends DefaultChargeUnit("exacoulomb", "EC", Nil, r"1" * r"1e18")
  final object zettacoulomb extends DefaultChargeUnit("zettacoulomb", "ZC", Nil, r"1" * r"1e21")
  final object yottacoulomb extends DefaultChargeUnit("yottacoulomb", "YC", Nil, r"1" * r"1e24")
  final object abcoulomb extends DefaultChargeUnit("abcoulomb", "abC", Nil, r"10")
  final object statcoulomb extends DefaultChargeUnit("statcoulomb", "statC", Seq("Fr", "esu"), Constants.SpeedOfLight) with NotExact
  final object atomic_unit_of_charge extends DefaultChargeUnit("atomic unit of charge", "au", Seq("e"), Constants.ElementaryCharge)

  def getSIUnit: ChargeUnit = coulomb

  def getUnits: Seq[ChargeUnit] =
    Seq(coulomb, yoctocoulomb, zeptocoulomb, attocoulomb, femtocoulomb, picocoulomb, nanocoulomb, microcoulomb, millicoulomb, centicoulomb, decicoulomb, decacoulomb, hectocoulomb, kilocoulomb, megacoulomb, gigacoulomb, teracoulomb, petacoulomb, exacoulomb, zettacoulomb, yottacoulomb, abcoulomb, statcoulomb, atomic_unit_of_charge)
}

object ChargeUnits{
  def C: ChargeUnit = ChargeUnitObjects.coulomb
  def yC: ChargeUnit = ChargeUnitObjects.yoctocoulomb
  def zC: ChargeUnit = ChargeUnitObjects.zeptocoulomb
  def aC: ChargeUnit = ChargeUnitObjects.attocoulomb
  def fC: ChargeUnit = ChargeUnitObjects.femtocoulomb
  def pC: ChargeUnit = ChargeUnitObjects.picocoulomb
  def nC: ChargeUnit = ChargeUnitObjects.nanocoulomb
  def μC: ChargeUnit = ChargeUnitObjects.microcoulomb
  def mcC: ChargeUnit = ChargeUnitObjects.microcoulomb
  def mC: ChargeUnit = ChargeUnitObjects.millicoulomb
  def cC: ChargeUnit = ChargeUnitObjects.centicoulomb
  def dC: ChargeUnit = ChargeUnitObjects.decicoulomb
  def daC: ChargeUnit = ChargeUnitObjects.decacoulomb
  def hC: ChargeUnit = ChargeUnitObjects.hectocoulomb
  def kC: ChargeUnit = ChargeUnitObjects.kilocoulomb
  def MC: ChargeUnit = ChargeUnitObjects.megacoulomb
  def GC: ChargeUnit = ChargeUnitObjects.gigacoulomb
  def TC: ChargeUnit = ChargeUnitObjects.teracoulomb
  def PC: ChargeUnit = ChargeUnitObjects.petacoulomb
  def EC: ChargeUnit = ChargeUnitObjects.exacoulomb
  def ZC: ChargeUnit = ChargeUnitObjects.zettacoulomb
  def YC: ChargeUnit = ChargeUnitObjects.yottacoulomb
  def abC: ChargeUnit = ChargeUnitObjects.abcoulomb
  def statC: ChargeUnit = ChargeUnitObjects.statcoulomb
  def Fr: ChargeUnit = ChargeUnitObjects.statcoulomb
  def esu: ChargeUnit = ChargeUnitObjects.statcoulomb
  def au: ChargeUnit = ChargeUnitObjects.atomic_unit_of_charge
  def e: ChargeUnit = ChargeUnitObjects.atomic_unit_of_charge

  def getSIUnit: ChargeUnit = ChargeUnitObjects.getSIUnit
  def getUnits: Seq[ChargeUnit] = ChargeUnitObjects.getUnits
}