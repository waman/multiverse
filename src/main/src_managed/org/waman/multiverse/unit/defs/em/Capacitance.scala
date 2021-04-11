package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.Constants

class Capacitance[A: Fractional](val value: A, val unit: CapacitanceUnit)
    extends LinearQuantity[Capacitance[A], A, CapacitanceUnit] {

  override protected def newQuantity(value: A, unit: CapacitanceUnit): Capacitance[A] = new Capacitance(value, unit)
}

trait CapacitanceUnit extends LinearUnit[CapacitanceUnit]{

  override def getSIUnit: CapacitanceUnit = CapacitanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = CapacitanceUnit.dimension
}

object CapacitanceUnit extends UnitInfo[CapacitanceUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 4, M -> -1, I -> 2, L -> -2).withDefaultValue(0)

  def getSIUnit: CapacitanceUnit = CapacitanceUnitObjects.farad
  import CapacitanceUnitObjects._

  def getUnits: Seq[CapacitanceUnit] =
    Seq(farad, yoctofarad, zeptofarad, attofarad, femtofarad, picofarad, nanofarad, microfarad, millifarad, centifarad, decifarad, decafarad, hectofarad, kilofarad, megafarad, gigafarad, terafarad, petafarad, exafarad, zettafarad, yottafarad, abfarad, statfarad)
}


/** For no aliase or user defined units */
class SimpleCapacitanceUnit(val name: String, val symbol: String, val interval: Real) extends CapacitanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultCapacitanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends CapacitanceUnit
  
object CapacitanceUnitObjects{

  final case object farad extends SimpleCapacitanceUnit("farad", "F", 1)
  final case object yoctofarad extends SimpleCapacitanceUnit("yoctofarad", "yF", r"1e-24")
  final case object zeptofarad extends SimpleCapacitanceUnit("zeptofarad", "zF", r"1e-21")
  final case object attofarad extends SimpleCapacitanceUnit("attofarad", "aF", r"1e-18")
  final case object femtofarad extends SimpleCapacitanceUnit("femtofarad", "fF", r"1e-15")
  final case object picofarad extends SimpleCapacitanceUnit("picofarad", "pF", r"1e-12")
  final case object nanofarad extends SimpleCapacitanceUnit("nanofarad", "nF", r"1e-9")
  final case object microfarad extends DefaultCapacitanceUnit("microfarad", "μF", Seq("mcF"), r"1e-6")
  final case object millifarad extends SimpleCapacitanceUnit("millifarad", "mF", r"1e-3")
  final case object centifarad extends SimpleCapacitanceUnit("centifarad", "cF", r"1e-2")
  final case object decifarad extends SimpleCapacitanceUnit("decifarad", "dF", r"1e-1")
  final case object decafarad extends SimpleCapacitanceUnit("decafarad", "daF", r"1e1")
  final case object hectofarad extends SimpleCapacitanceUnit("hectofarad", "hF", r"1e2")
  final case object kilofarad extends DefaultCapacitanceUnit("kilofarad", "kF", Seq("KF"), r"1e3")
  final case object megafarad extends SimpleCapacitanceUnit("megafarad", "MF", r"1e6")
  final case object gigafarad extends SimpleCapacitanceUnit("gigafarad", "GF", r"1e9")
  final case object terafarad extends SimpleCapacitanceUnit("terafarad", "TF", r"1e12")
  final case object petafarad extends SimpleCapacitanceUnit("petafarad", "PF", r"1e15")
  final case object exafarad extends SimpleCapacitanceUnit("exafarad", "EF", r"1e18")
  final case object zettafarad extends SimpleCapacitanceUnit("zettafarad", "ZF", r"1e21")
  final case object yottafarad extends SimpleCapacitanceUnit("yottafarad", "YF", r"1e24")
  final case object abfarad extends SimpleCapacitanceUnit("abfarad", "abF", r"1e9")
  final case object statfarad extends SimpleCapacitanceUnit("statfarad", "statF", Constants.SpeedOfLight * Constants.SpeedOfLight * r"1e-5") with Description {
    def description: String = "Formal unit for Gaussian and ESU CGS unit system."
  }
}


object CapacitanceUnits{

  /** farad */
  def F: CapacitanceUnit = CapacitanceUnitObjects.farad
  /** yoctofarad */
  def yF: CapacitanceUnit = CapacitanceUnitObjects.yoctofarad
  /** zeptofarad */
  def zF: CapacitanceUnit = CapacitanceUnitObjects.zeptofarad
  /** attofarad */
  def aF: CapacitanceUnit = CapacitanceUnitObjects.attofarad
  /** femtofarad */
  def fF: CapacitanceUnit = CapacitanceUnitObjects.femtofarad
  /** picofarad */
  def pF: CapacitanceUnit = CapacitanceUnitObjects.picofarad
  /** nanofarad */
  def nF: CapacitanceUnit = CapacitanceUnitObjects.nanofarad
  /** microfarad */
  def μF: CapacitanceUnit = CapacitanceUnitObjects.microfarad
  /** microfarad */
  def mcF: CapacitanceUnit = CapacitanceUnitObjects.microfarad
  /** millifarad */
  def mF: CapacitanceUnit = CapacitanceUnitObjects.millifarad
  /** centifarad */
  def cF: CapacitanceUnit = CapacitanceUnitObjects.centifarad
  /** decifarad */
  def dF: CapacitanceUnit = CapacitanceUnitObjects.decifarad
  /** decafarad */
  def daF: CapacitanceUnit = CapacitanceUnitObjects.decafarad
  /** hectofarad */
  def hF: CapacitanceUnit = CapacitanceUnitObjects.hectofarad
  /** kilofarad */
  def kF: CapacitanceUnit = CapacitanceUnitObjects.kilofarad
  /** kilofarad */
  def KF: CapacitanceUnit = CapacitanceUnitObjects.kilofarad
  /** megafarad */
  def MF: CapacitanceUnit = CapacitanceUnitObjects.megafarad
  /** gigafarad */
  def GF: CapacitanceUnit = CapacitanceUnitObjects.gigafarad
  /** terafarad */
  def TF: CapacitanceUnit = CapacitanceUnitObjects.terafarad
  /** petafarad */
  def PF: CapacitanceUnit = CapacitanceUnitObjects.petafarad
  /** exafarad */
  def EF: CapacitanceUnit = CapacitanceUnitObjects.exafarad
  /** zettafarad */
  def ZF: CapacitanceUnit = CapacitanceUnitObjects.zettafarad
  /** yottafarad */
  def YF: CapacitanceUnit = CapacitanceUnitObjects.yottafarad
  /** abfarad */
  def abF: CapacitanceUnit = CapacitanceUnitObjects.abfarad
  /** statfarad */
  def statF: CapacitanceUnit = CapacitanceUnitObjects.statfarad
}