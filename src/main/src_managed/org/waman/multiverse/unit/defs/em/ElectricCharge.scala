package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.ra._
import org.waman.multiverse.Constants

class ElectricCharge[A: Fractional](val value: A, val unit: ElectricChargeUnit)
    extends LinearQuantity[ElectricCharge[A], A, ElectricChargeUnit] {

  override protected def newQuantity(value: A, unit: ElectricChargeUnit): ElectricCharge[A] = new ElectricCharge(value, unit)

  def /(voltage: Voltage[A]): Capacitance[A] = new Capacitance(this.value / voltage.value, this.unit / voltage.unit)

  def /(time: Time[A]): ElectricCurrent[A] = new ElectricCurrent(this.value / time.value, this.unit / time.unit)

  def *(length: Length[A]): ElectricDipole[A] = new ElectricDipole(this.value * length.value, this.unit * length.unit)

  def /(mass: Mass[A]): Exposure[A] = new Exposure(this.value / mass.value, this.unit / mass.unit)
}

/** None */
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


/** For no alias or user defined units */
class SimpleElectricChargeUnit(val name: String, val symbol: String, val interval: Real) extends ElectricChargeUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultElectricChargeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ElectricChargeUnit
  
object ElectricChargeUnitObjects{

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

  /** coulomb */
  def C: ElectricChargeUnit = ElectricChargeUnitObjects.coulomb
  /** yoctocoulomb */
  def yC: ElectricChargeUnit = ElectricChargeUnitObjects.yoctocoulomb
  /** zeptocoulomb */
  def zC: ElectricChargeUnit = ElectricChargeUnitObjects.zeptocoulomb
  /** attocoulomb */
  def aC: ElectricChargeUnit = ElectricChargeUnitObjects.attocoulomb
  /** femtocoulomb */
  def fC: ElectricChargeUnit = ElectricChargeUnitObjects.femtocoulomb
  /** picocoulomb */
  def pC: ElectricChargeUnit = ElectricChargeUnitObjects.picocoulomb
  /** nanocoulomb */
  def nC: ElectricChargeUnit = ElectricChargeUnitObjects.nanocoulomb
  /** microcoulomb */
  def μC: ElectricChargeUnit = ElectricChargeUnitObjects.microcoulomb
  /** microcoulomb */
  def mcC: ElectricChargeUnit = ElectricChargeUnitObjects.microcoulomb
  /** millicoulomb */
  def mC: ElectricChargeUnit = ElectricChargeUnitObjects.millicoulomb
  /** centicoulomb */
  def cC: ElectricChargeUnit = ElectricChargeUnitObjects.centicoulomb
  /** decicoulomb */
  def dC: ElectricChargeUnit = ElectricChargeUnitObjects.decicoulomb
  /** decacoulomb */
  def daC: ElectricChargeUnit = ElectricChargeUnitObjects.decacoulomb
  /** hectocoulomb */
  def hC: ElectricChargeUnit = ElectricChargeUnitObjects.hectocoulomb
  /** kilocoulomb */
  def kC: ElectricChargeUnit = ElectricChargeUnitObjects.kilocoulomb
  /** kilocoulomb */
  def KC: ElectricChargeUnit = ElectricChargeUnitObjects.kilocoulomb
  /** megacoulomb */
  def MC: ElectricChargeUnit = ElectricChargeUnitObjects.megacoulomb
  /** gigacoulomb */
  def GC: ElectricChargeUnit = ElectricChargeUnitObjects.gigacoulomb
  /** teracoulomb */
  def TC: ElectricChargeUnit = ElectricChargeUnitObjects.teracoulomb
  /** petacoulomb */
  def PC: ElectricChargeUnit = ElectricChargeUnitObjects.petacoulomb
  /** exacoulomb */
  def EC: ElectricChargeUnit = ElectricChargeUnitObjects.exacoulomb
  /** zettacoulomb */
  def ZC: ElectricChargeUnit = ElectricChargeUnitObjects.zettacoulomb
  /** yottacoulomb */
  def YC: ElectricChargeUnit = ElectricChargeUnitObjects.yottacoulomb
  /** atomic unit of charge */
  def au: ElectricChargeUnit = ElectricChargeUnitObjects.atomic_unit_of_charge
  /** atomic unit of charge */
  def e: ElectricChargeUnit = ElectricChargeUnitObjects.atomic_unit_of_charge
  /** abcoulomb */
  def abC: ElectricChargeUnit = ElectricChargeUnitObjects.abcoulomb
  /** abcoulomb */
  def emu: ElectricChargeUnit = ElectricChargeUnitObjects.abcoulomb
  /** statcoulomb */
  def statC: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  /** statcoulomb */
  def Fr: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  /** statcoulomb */
  def esu: ElectricChargeUnit = ElectricChargeUnitObjects.statcoulomb
  /** faraday */
  def F: ElectricChargeUnit = ElectricChargeUnitObjects.faraday
}