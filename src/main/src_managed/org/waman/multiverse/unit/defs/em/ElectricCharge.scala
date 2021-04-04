package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._

import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.radioactivity._
import org.waman.multiverse.Constants

class ElectricCharge[A: Fractional](val value: A, val unit: ElectricChargeUnit)
    extends LinearQuantity[ElectricCharge[A], A, ElectricChargeUnit] {

  override protected def newQuantity(value: A, unit: ElectricChargeUnit): ElectricCharge[A] = new ElectricCharge(value, unit)

  def /(voltage: Voltage[A]): Capacitance[A] = new Capacitance(this.value / voltage.value, this.unit / voltage.unit)

  def /(time: Time[A]): ElectricCurrent[A] = new ElectricCurrent(this.value / time.value, this.unit / time.unit)

  def *(length: Length[A]): ElectricDipole[A] = new ElectricDipole(this.value * length.value, this.unit * length.unit)

  def /(mass: Mass[A]): Exposure[A] = new Exposure(this.value / mass.value, this.unit / mass.unit)
}

trait ElectricChargeUnit extends LinearUnit[ElectricChargeUnit]{

  override def getSIUnit: ElectricChargeUnit = ElectricChargeUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ElectricChargeUnit.dimension

  def /(voltageUnit: VoltageUnit): CapacitanceUnit =
    new QuotientUnit[CapacitanceUnit, ElectricChargeUnit, VoltageUnit](ElectricChargeUnit.this, voltageUnit) with CapacitanceUnit

  def /(timeUnit: TimeUnit): ElectricCurrentUnit =
    new QuotientUnit[ElectricCurrentUnit, ElectricChargeUnit, TimeUnit](ElectricChargeUnit.this, timeUnit) with ElectricCurrentUnit

  def *(lengthUnit: LengthUnit): ElectricDipoleUnit =
    new ProductUnit[ElectricDipoleUnit, ElectricChargeUnit, LengthUnit](ElectricChargeUnit.this, lengthUnit) with ElectricDipoleUnit

  def /(massUnit: MassUnit): ExposureUnit =
    new QuotientUnit[ExposureUnit, ElectricChargeUnit, MassUnit](ElectricChargeUnit.this, massUnit) with ExposureUnit
}

object ElectricChargeUnit extends UnitInfo[ElectricChargeUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, I -> 1).withDefaultValue(0)

  def getSIUnit: ElectricChargeUnit = ElectricChargeUnitObjects.coulomb
  import ElectricChargeUnitObjects._

  def getUnits: Seq[ElectricChargeUnit] =
    Seq(coulomb, yoctocoulomb, zeptocoulomb, attocoulomb, femtocoulomb, picocoulomb, nanocoulomb, microcoulomb, millicoulomb, centicoulomb, decicoulomb, decacoulomb, hectocoulomb, kilocoulomb, megacoulomb, gigacoulomb, teracoulomb, petacoulomb, exacoulomb, zettacoulomb, yottacoulomb, atomic_unit_of_charge, abcoulomb, statcoulomb, faraday)
}


/** For no aliase or user defined units */
class SimpleElectricChargeUnit(val name: String, val symbol: String, val interval: Real) extends ElectricChargeUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultElectricChargeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ElectricChargeUnit
  
object ElectricChargeUnitObjects{
  import spire.implicits._

  final case object coulomb extends SimpleElectricChargeUnit("coulomb", "C", 1)
  final case object yoctocoulomb extends SimpleElectricChargeUnit("yoctocoulomb", "yC", r"1e-24")
  final case object zeptocoulomb extends SimpleElectricChargeUnit("zeptocoulomb", "zC", r"1e-21")
  final case object attocoulomb extends SimpleElectricChargeUnit("attocoulomb", "aC", r"1e-18")
  final case object femtocoulomb extends SimpleElectricChargeUnit("femtocoulomb", "fC", r"1e-15")
  final case object picocoulomb extends SimpleElectricChargeUnit("picocoulomb", "pC", r"1e-12")
  final case object nanocoulomb extends SimpleElectricChargeUnit("nanocoulomb", "nC", r"1e-9")
  final case object microcoulomb extends DefaultElectricChargeUnit("microcoulomb", "μC", Seq("mcC"), r"1e-6")
  final case object millicoulomb extends SimpleElectricChargeUnit("millicoulomb", "mC", r"1e-3")
  final case object centicoulomb extends SimpleElectricChargeUnit("centicoulomb", "cC", r"1e-2")
  final case object decicoulomb extends SimpleElectricChargeUnit("decicoulomb", "dC", r"1e-1")
  final case object decacoulomb extends SimpleElectricChargeUnit("decacoulomb", "daC", r"1e1")
  final case object hectocoulomb extends SimpleElectricChargeUnit("hectocoulomb", "hC", r"1e2")
  final case object kilocoulomb extends DefaultElectricChargeUnit("kilocoulomb", "kC", Seq("KC"), r"1e3")
  final case object megacoulomb extends SimpleElectricChargeUnit("megacoulomb", "MC", r"1e6")
  final case object gigacoulomb extends SimpleElectricChargeUnit("gigacoulomb", "GC", r"1e9")
  final case object teracoulomb extends SimpleElectricChargeUnit("teracoulomb", "TC", r"1e12")
  final case object petacoulomb extends SimpleElectricChargeUnit("petacoulomb", "PC", r"1e15")
  final case object exacoulomb extends SimpleElectricChargeUnit("exacoulomb", "EC", r"1e18")
  final case object zettacoulomb extends SimpleElectricChargeUnit("zettacoulomb", "ZC", r"1e21")
  final case object yottacoulomb extends SimpleElectricChargeUnit("yottacoulomb", "YC", r"1e24")
  final case object atomic_unit_of_charge extends DefaultElectricChargeUnit("atomic unit of charge", "au", Seq("e"), Constants.ElementaryCharge)
  final case object abcoulomb extends DefaultElectricChargeUnit("abcoulomb", "abC", Seq("emu"), r"10")
  final case object statcoulomb extends DefaultElectricChargeUnit("statcoulomb", "statC", Seq("Fr", "esu"), r"0.1" / Constants.SpeedOfLight)
  final case object faraday extends SimpleElectricChargeUnit("faraday", "F", Constants.AvogadroConstant * Constants.ElementaryCharge)
}


object ElectricChargeUnits{

  def C: ElectricChargeUnit = ElectricChargeUnitObjects.coulomb
  def yC: ElectricChargeUnit = ElectricChargeUnitObjects.yoctocoulomb
  def zC: ElectricChargeUnit = ElectricChargeUnitObjects.zeptocoulomb
  def aC: ElectricChargeUnit = ElectricChargeUnitObjects.attocoulomb
  def fC: ElectricChargeUnit = ElectricChargeUnitObjects.femtocoulomb
  def pC: ElectricChargeUnit = ElectricChargeUnitObjects.picocoulomb
  def nC: ElectricChargeUnit = ElectricChargeUnitObjects.nanocoulomb
  def μC: ElectricChargeUnit = ElectricChargeUnitObjects.microcoulomb
  def mcC: ElectricChargeUnit = ElectricChargeUnitObjects.microcoulomb
  def mC: ElectricChargeUnit = ElectricChargeUnitObjects.millicoulomb
  def cC: ElectricChargeUnit = ElectricChargeUnitObjects.centicoulomb
  def dC: ElectricChargeUnit = ElectricChargeUnitObjects.decicoulomb
  def daC: ElectricChargeUnit = ElectricChargeUnitObjects.decacoulomb
  def hC: ElectricChargeUnit = ElectricChargeUnitObjects.hectocoulomb
  def kC: ElectricChargeUnit = ElectricChargeUnitObjects.kilocoulomb
  def KC: ElectricChargeUnit = ElectricChargeUnitObjects.kilocoulomb
  def MC: ElectricChargeUnit = ElectricChargeUnitObjects.megacoulomb
  def GC: ElectricChargeUnit = ElectricChargeUnitObjects.gigacoulomb
  def TC: ElectricChargeUnit = ElectricChargeUnitObjects.teracoulomb
  def PC: ElectricChargeUnit = ElectricChargeUnitObjects.petacoulomb
  def EC: ElectricChargeUnit = ElectricChargeUnitObjects.exacoulomb
  def ZC: ElectricChargeUnit = ElectricChargeUnitObjects.zettacoulomb
  def YC: ElectricChargeUnit = ElectricChargeUnitObjects.yottacoulomb
  def au: ElectricChargeUnit = ElectricChargeUnitObjects.atomic_unit_of_charge
  def e: ElectricChargeUnit = ElectricChargeUnitObjects.atomic_unit_of_charge
  def abC: ElectricChargeUnit = ElectricChargeUnitObjects.abcoulomb
  def emu: ElectricChargeUnit = ElectricChargeUnitObjects.abcoulomb
  def statC: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  def Fr: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  def esu: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  def F: ElectricChargeUnit = ElectricChargeUnitObjects.faraday
}