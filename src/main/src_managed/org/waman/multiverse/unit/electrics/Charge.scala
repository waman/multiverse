package org.waman.multiverse.unit.electrics

import spire.math.Real
import spire.math.Fractional

import org.waman.multiverse._


import org.waman.multiverse.unit.basic.Length
import org.waman.multiverse.unit.basic.LengthUnit


import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit


import org.waman.multiverse.unit.basic.Mass
import org.waman.multiverse.unit.basic.MassUnit


import org.waman.multiverse.unit.radioactivity.Exposure
import org.waman.multiverse.unit.radioactivity.ExposureUnit


class Charge[A: Fractional](val value: A, val unit: ChargeUnit)
    extends LinearQuantity[Charge[A], A, ChargeUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: ChargeUnit): Charge[A] = new Charge(value, unit)

  def *(length: Length[A]): Dipole[A] = new Dipole(this.value * length.value, this.unit * length.unit)

  def /(voltage: Voltage[A]): Capacitance[A] = new Capacitance(this.value / voltage.value, this.unit / voltage.unit)

  def /(time: Time[A]): Current[A] = new Current(this.value / time.value, this.unit / time.unit)

  def /(mass: Mass[A]): Exposure[A] = new Exposure(this.value / mass.value, this.unit / mass.unit)
}

trait ChargeUnit extends LinearUnit[ChargeUnit]{

  override def getSIUnit: ChargeUnit = ChargeUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ChargeUnit.dimension

  def *(lengthUnit: LengthUnit): DipoleUnit =
    new AbstractProductUnit[DipoleUnit, ChargeUnit, LengthUnit](ChargeUnit.this, lengthUnit) with DipoleUnit

  def /(voltageUnit: VoltageUnit): CapacitanceUnit =
    new AbstractQuotientUnit[CapacitanceUnit, ChargeUnit, VoltageUnit](ChargeUnit.this, voltageUnit) with CapacitanceUnit

  def /(timeUnit: TimeUnit): CurrentUnit =
    new AbstractQuotientUnit[CurrentUnit, ChargeUnit, TimeUnit](ChargeUnit.this, timeUnit) with CurrentUnit

  def /(massUnit: MassUnit): ExposureUnit =
    new AbstractQuotientUnit[ExposureUnit, ChargeUnit, MassUnit](ChargeUnit.this, massUnit) with ExposureUnit
}

object ChargeUnit extends UnitInfo[ChargeUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, I -> 1).withDefaultValue(0)

  def getSIUnit: ChargeUnit = ChargeUnitObjects.coulomb

  import ChargeUnitObjects._
  def getUnits: Seq[ChargeUnit] =
    Seq(coulomb, yoctocoulomb, zeptocoulomb, attocoulomb, femtocoulomb, picocoulomb, nanocoulomb, microcoulomb, millicoulomb, centicoulomb, decicoulomb, decacoulomb, hectocoulomb, kilocoulomb, megacoulomb, gigacoulomb, teracoulomb, petacoulomb, exacoulomb, zettacoulomb, yottacoulomb, abcoulomb, statcoulomb, atomic_unit_of_charge)
}

/** For no aliase or user defined units */
class SimpleChargeUnit(val name: String, val symbol: String, val interval: Real) extends ChargeUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultChargeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ChargeUnit

object ChargeUnitObjects{

  import spire.implicits._

  import org.waman.multiverse.unit.Constants

  final case object coulomb extends SimpleChargeUnit("coulomb", "C", 1)
  final case object yoctocoulomb extends SimpleChargeUnit("yoctocoulomb", "yC", r"1e-24")
  final case object zeptocoulomb extends SimpleChargeUnit("zeptocoulomb", "zC", r"1e-21")
  final case object attocoulomb extends SimpleChargeUnit("attocoulomb", "aC", r"1e-18")
  final case object femtocoulomb extends SimpleChargeUnit("femtocoulomb", "fC", r"1e-15")
  final case object picocoulomb extends SimpleChargeUnit("picocoulomb", "pC", r"1e-12")
  final case object nanocoulomb extends SimpleChargeUnit("nanocoulomb", "nC", r"1e-9")
  final case object microcoulomb extends DefaultChargeUnit("microcoulomb", "μC", Seq("mcC"), r"1e-6")
  final case object millicoulomb extends SimpleChargeUnit("millicoulomb", "mC", r"1e-3")
  final case object centicoulomb extends SimpleChargeUnit("centicoulomb", "cC", r"1e-2")
  final case object decicoulomb extends SimpleChargeUnit("decicoulomb", "dC", r"1e-1")
  final case object decacoulomb extends SimpleChargeUnit("decacoulomb", "daC", r"1e1")
  final case object hectocoulomb extends SimpleChargeUnit("hectocoulomb", "hC", r"1e2")
  final case object kilocoulomb extends DefaultChargeUnit("kilocoulomb", "kC", Seq("KC"), r"1e3")
  final case object megacoulomb extends SimpleChargeUnit("megacoulomb", "MC", r"1e6")
  final case object gigacoulomb extends SimpleChargeUnit("gigacoulomb", "GC", r"1e9")
  final case object teracoulomb extends SimpleChargeUnit("teracoulomb", "TC", r"1e12")
  final case object petacoulomb extends SimpleChargeUnit("petacoulomb", "PC", r"1e15")
  final case object exacoulomb extends SimpleChargeUnit("exacoulomb", "EC", r"1e18")
  final case object zettacoulomb extends SimpleChargeUnit("zettacoulomb", "ZC", r"1e21")
  final case object yottacoulomb extends SimpleChargeUnit("yottacoulomb", "YC", r"1e24")
  final case object abcoulomb extends SimpleChargeUnit("abcoulomb", "abC", r"10")
  final case object statcoulomb extends DefaultChargeUnit("statcoulomb", "statC", Seq("Fr", "esu"), Constants.SpeedOfLight) with NotExact
  final case object atomic_unit_of_charge extends DefaultChargeUnit("atomic unit of charge", "au", Seq("e"), Constants.ElementaryCharge)
}

object ChargeUnits{

  def C: ChargeUnit = ChargeUnitObjects.coulomb
  def yC: ChargeUnit = ChargeUnitObjects.yoctocoulomb
  def zC: ChargeUnit = ChargeUnitObjects.zeptocoulomb
  def aC: ChargeUnit = ChargeUnitObjects.attocoulomb
  def fC: ChargeUnit = ChargeUnitObjects.femtocoulomb
  def pC: ChargeUnit = ChargeUnitObjects.picocoulomb
  def nC: ChargeUnit = ChargeUnitObjects.nanocoulomb
  def `μC`: ChargeUnit = ChargeUnitObjects.microcoulomb
  def mcC: ChargeUnit = ChargeUnitObjects.microcoulomb
  def mC: ChargeUnit = ChargeUnitObjects.millicoulomb
  def cC: ChargeUnit = ChargeUnitObjects.centicoulomb
  def dC: ChargeUnit = ChargeUnitObjects.decicoulomb
  def daC: ChargeUnit = ChargeUnitObjects.decacoulomb
  def hC: ChargeUnit = ChargeUnitObjects.hectocoulomb
  def kC: ChargeUnit = ChargeUnitObjects.kilocoulomb
  def KC: ChargeUnit = ChargeUnitObjects.kilocoulomb
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
}