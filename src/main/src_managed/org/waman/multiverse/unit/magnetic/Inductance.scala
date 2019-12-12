package org.waman.multiverse.unit.magnetic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class Inductance[A: Fractional](val value: A, val unit: InductanceUnit)
    extends LinearQuantity[Inductance[A], A, InductanceUnit] {

  override protected def newQuantity(value: A, unit: InductanceUnit): Inductance[A] = new Inductance(value, unit)
}

trait InductanceUnit extends LinearUnit[InductanceUnit]{

  override def getSIUnit: InductanceUnit = InductanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = InductanceUnit.dimension

}

object InductanceUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, I -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: InductanceUnit = InductanceUnitObjects.henry

  import InductanceUnitObjects._
  def getUnits: Seq[InductanceUnit] =
    Seq(henry, yoctohenry, zeptohenry, attohenry, femtohenry, picohenry, nanohenry, microhenry, millihenry, centihenry, decihenry, decahenry, hectohenry, kilohenry, megahenry, gigahenry, terahenry, petahenry, exahenry, zettahenry, yottahenry)
}



class DefaultInductanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends InductanceUnit

object InductanceUnitObjects{

  final object henry extends DefaultInductanceUnit("henry", "H", Nil, 1)
  final object yoctohenry extends DefaultInductanceUnit("yoctohenry", "yH", Nil, 1 * r"1e-24")
  final object zeptohenry extends DefaultInductanceUnit("zeptohenry", "zH", Nil, 1 * r"1e-21")
  final object attohenry extends DefaultInductanceUnit("attohenry", "aH", Nil, 1 * r"1e-18")
  final object femtohenry extends DefaultInductanceUnit("femtohenry", "fH", Nil, 1 * r"1e-15")
  final object picohenry extends DefaultInductanceUnit("picohenry", "pH", Nil, 1 * r"1e-12")
  final object nanohenry extends DefaultInductanceUnit("nanohenry", "nH", Nil, 1 * r"1e-9")
  final object microhenry extends DefaultInductanceUnit("microhenry", "μH", Seq("mcH"), 1 * r"1e-6")
  final object millihenry extends DefaultInductanceUnit("millihenry", "mH", Nil, 1 * r"1e-3")
  final object centihenry extends DefaultInductanceUnit("centihenry", "cH", Nil, 1 * r"1e-2")
  final object decihenry extends DefaultInductanceUnit("decihenry", "dH", Nil, 1 * r"1e-1")
  final object decahenry extends DefaultInductanceUnit("decahenry", "daH", Nil, 1 * r"1e1")
  final object hectohenry extends DefaultInductanceUnit("hectohenry", "hH", Nil, 1 * r"1e2")
  final object kilohenry extends DefaultInductanceUnit("kilohenry", "kH", Seq("KH"), 1 * r"1e3")
  final object megahenry extends DefaultInductanceUnit("megahenry", "MH", Nil, 1 * r"1e6")
  final object gigahenry extends DefaultInductanceUnit("gigahenry", "GH", Nil, 1 * r"1e9")
  final object terahenry extends DefaultInductanceUnit("terahenry", "TH", Nil, 1 * r"1e12")
  final object petahenry extends DefaultInductanceUnit("petahenry", "PH", Nil, 1 * r"1e15")
  final object exahenry extends DefaultInductanceUnit("exahenry", "EH", Nil, 1 * r"1e18")
  final object zettahenry extends DefaultInductanceUnit("zettahenry", "ZH", Nil, 1 * r"1e21")
  final object yottahenry extends DefaultInductanceUnit("yottahenry", "YH", Nil, 1 * r"1e24")
}

object InductanceUnits{
  def H: InductanceUnit = InductanceUnitObjects.henry
  def yH: InductanceUnit = InductanceUnitObjects.yoctohenry
  def zH: InductanceUnit = InductanceUnitObjects.zeptohenry
  def aH: InductanceUnit = InductanceUnitObjects.attohenry
  def fH: InductanceUnit = InductanceUnitObjects.femtohenry
  def pH: InductanceUnit = InductanceUnitObjects.picohenry
  def nH: InductanceUnit = InductanceUnitObjects.nanohenry
  def μH: InductanceUnit = InductanceUnitObjects.microhenry
  def mcH: InductanceUnit = InductanceUnitObjects.microhenry
  def mH: InductanceUnit = InductanceUnitObjects.millihenry
  def cH: InductanceUnit = InductanceUnitObjects.centihenry
  def dH: InductanceUnit = InductanceUnitObjects.decihenry
  def daH: InductanceUnit = InductanceUnitObjects.decahenry
  def hH: InductanceUnit = InductanceUnitObjects.hectohenry
  def kH: InductanceUnit = InductanceUnitObjects.kilohenry
  def KH: InductanceUnit = InductanceUnitObjects.kilohenry
  def MH: InductanceUnit = InductanceUnitObjects.megahenry
  def GH: InductanceUnit = InductanceUnitObjects.gigahenry
  def TH: InductanceUnit = InductanceUnitObjects.terahenry
  def PH: InductanceUnit = InductanceUnitObjects.petahenry
  def EH: InductanceUnit = InductanceUnitObjects.exahenry
  def ZH: InductanceUnit = InductanceUnitObjects.zettahenry
  def YH: InductanceUnit = InductanceUnitObjects.yottahenry
}