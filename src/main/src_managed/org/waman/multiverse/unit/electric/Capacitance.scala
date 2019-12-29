package org.waman.multiverse.unit.electric

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Capacitance[A: Fractional](val value: A, val unit: CapacitanceUnit)
    extends LinearQuantity[Capacitance[A], A, CapacitanceUnit] {

  override protected def newQuantity(value: A, unit: CapacitanceUnit): Capacitance[A] = new Capacitance(value, unit)

}

trait CapacitanceUnit extends LinearUnit[CapacitanceUnit]{

  override def getSIUnit: CapacitanceUnit = CapacitanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = CapacitanceUnit.dimension

}

object CapacitanceUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 4, M -> -1, I -> 2, L -> -2).withDefaultValue(0)

  def getSIUnit: CapacitanceUnit = CapacitanceUnitObjects.farad

  import CapacitanceUnitObjects._
  def getUnits: Seq[CapacitanceUnit] =
    Seq(farad, yoctofarad, zeptofarad, attofarad, femtofarad, picofarad, nanofarad, microfarad, millifarad, centifarad, decifarad, decafarad, hectofarad, kilofarad, megafarad, gigafarad, terafarad, petafarad, exafarad, zettafarad, yottafarad)
}

class DefaultCapacitanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends CapacitanceUnit

object CapacitanceUnitObjects{

  final object farad extends DefaultCapacitanceUnit("farad", "F", Nil, 1)
  final object yoctofarad extends DefaultCapacitanceUnit("yoctofarad", "yF", Nil, 1 * r"1e-24")
  final object zeptofarad extends DefaultCapacitanceUnit("zeptofarad", "zF", Nil, 1 * r"1e-21")
  final object attofarad extends DefaultCapacitanceUnit("attofarad", "aF", Nil, 1 * r"1e-18")
  final object femtofarad extends DefaultCapacitanceUnit("femtofarad", "fF", Nil, 1 * r"1e-15")
  final object picofarad extends DefaultCapacitanceUnit("picofarad", "pF", Nil, 1 * r"1e-12")
  final object nanofarad extends DefaultCapacitanceUnit("nanofarad", "nF", Nil, 1 * r"1e-9")
  final object microfarad extends DefaultCapacitanceUnit("microfarad", "μF", Seq("mcF"), 1 * r"1e-6")
  final object millifarad extends DefaultCapacitanceUnit("millifarad", "mF", Nil, 1 * r"1e-3")
  final object centifarad extends DefaultCapacitanceUnit("centifarad", "cF", Nil, 1 * r"1e-2")
  final object decifarad extends DefaultCapacitanceUnit("decifarad", "dF", Nil, 1 * r"1e-1")
  final object decafarad extends DefaultCapacitanceUnit("decafarad", "daF", Nil, 1 * r"1e1")
  final object hectofarad extends DefaultCapacitanceUnit("hectofarad", "hF", Nil, 1 * r"1e2")
  final object kilofarad extends DefaultCapacitanceUnit("kilofarad", "kF", Seq("KF"), 1 * r"1e3")
  final object megafarad extends DefaultCapacitanceUnit("megafarad", "MF", Nil, 1 * r"1e6")
  final object gigafarad extends DefaultCapacitanceUnit("gigafarad", "GF", Nil, 1 * r"1e9")
  final object terafarad extends DefaultCapacitanceUnit("terafarad", "TF", Nil, 1 * r"1e12")
  final object petafarad extends DefaultCapacitanceUnit("petafarad", "PF", Nil, 1 * r"1e15")
  final object exafarad extends DefaultCapacitanceUnit("exafarad", "EF", Nil, 1 * r"1e18")
  final object zettafarad extends DefaultCapacitanceUnit("zettafarad", "ZF", Nil, 1 * r"1e21")
  final object yottafarad extends DefaultCapacitanceUnit("yottafarad", "YF", Nil, 1 * r"1e24")
}

object CapacitanceUnits{
  def F: CapacitanceUnit = CapacitanceUnitObjects.farad
  def yF: CapacitanceUnit = CapacitanceUnitObjects.yoctofarad
  def zF: CapacitanceUnit = CapacitanceUnitObjects.zeptofarad
  def aF: CapacitanceUnit = CapacitanceUnitObjects.attofarad
  def fF: CapacitanceUnit = CapacitanceUnitObjects.femtofarad
  def pF: CapacitanceUnit = CapacitanceUnitObjects.picofarad
  def nF: CapacitanceUnit = CapacitanceUnitObjects.nanofarad
  def μF: CapacitanceUnit = CapacitanceUnitObjects.microfarad
  def mcF: CapacitanceUnit = CapacitanceUnitObjects.microfarad
  def mF: CapacitanceUnit = CapacitanceUnitObjects.millifarad
  def cF: CapacitanceUnit = CapacitanceUnitObjects.centifarad
  def dF: CapacitanceUnit = CapacitanceUnitObjects.decifarad
  def daF: CapacitanceUnit = CapacitanceUnitObjects.decafarad
  def hF: CapacitanceUnit = CapacitanceUnitObjects.hectofarad
  def kF: CapacitanceUnit = CapacitanceUnitObjects.kilofarad
  def KF: CapacitanceUnit = CapacitanceUnitObjects.kilofarad
  def MF: CapacitanceUnit = CapacitanceUnitObjects.megafarad
  def GF: CapacitanceUnit = CapacitanceUnitObjects.gigafarad
  def TF: CapacitanceUnit = CapacitanceUnitObjects.terafarad
  def PF: CapacitanceUnit = CapacitanceUnitObjects.petafarad
  def EF: CapacitanceUnit = CapacitanceUnitObjects.exafarad
  def ZF: CapacitanceUnit = CapacitanceUnitObjects.zettafarad
  def YF: CapacitanceUnit = CapacitanceUnitObjects.yottafarad
}