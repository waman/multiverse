package org.waman.multiverse.unit.magnetics

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

object InductanceUnit extends UnitInfo[InductanceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, I -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: InductanceUnit = InductanceUnitObjects.henry

  import InductanceUnitObjects._
  def getUnits: Seq[InductanceUnit] =
    Seq(henry, yoctohenry, zeptohenry, attohenry, femtohenry, picohenry, nanohenry, microhenry, millihenry, centihenry, decihenry, decahenry, hectohenry, kilohenry, megahenry, gigahenry, terahenry, petahenry, exahenry, zettahenry, yottahenry)
}

/** For user defined units */
class SimpleInductanceUnit(val name: String, val symbol: String, val interval: Real) extends InductanceUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultInductanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends InductanceUnit

object InductanceUnitObjects{

  final case object henry extends DefaultInductanceUnit("henry", "H", Nil, 1)
  final case object yoctohenry extends DefaultInductanceUnit("yoctohenry", "yH", Nil, r"1e-24")
  final case object zeptohenry extends DefaultInductanceUnit("zeptohenry", "zH", Nil, r"1e-21")
  final case object attohenry extends DefaultInductanceUnit("attohenry", "aH", Nil, r"1e-18")
  final case object femtohenry extends DefaultInductanceUnit("femtohenry", "fH", Nil, r"1e-15")
  final case object picohenry extends DefaultInductanceUnit("picohenry", "pH", Nil, r"1e-12")
  final case object nanohenry extends DefaultInductanceUnit("nanohenry", "nH", Nil, r"1e-9")
  final case object microhenry extends DefaultInductanceUnit("microhenry", "μH", Seq("mcH"), r"1e-6")
  final case object millihenry extends DefaultInductanceUnit("millihenry", "mH", Nil, r"1e-3")
  final case object centihenry extends DefaultInductanceUnit("centihenry", "cH", Nil, r"1e-2")
  final case object decihenry extends DefaultInductanceUnit("decihenry", "dH", Nil, r"1e-1")
  final case object decahenry extends DefaultInductanceUnit("decahenry", "daH", Nil, r"1e1")
  final case object hectohenry extends DefaultInductanceUnit("hectohenry", "hH", Nil, r"1e2")
  final case object kilohenry extends DefaultInductanceUnit("kilohenry", "kH", Seq("KH"), r"1e3")
  final case object megahenry extends DefaultInductanceUnit("megahenry", "MH", Nil, r"1e6")
  final case object gigahenry extends DefaultInductanceUnit("gigahenry", "GH", Nil, r"1e9")
  final case object terahenry extends DefaultInductanceUnit("terahenry", "TH", Nil, r"1e12")
  final case object petahenry extends DefaultInductanceUnit("petahenry", "PH", Nil, r"1e15")
  final case object exahenry extends DefaultInductanceUnit("exahenry", "EH", Nil, r"1e18")
  final case object zettahenry extends DefaultInductanceUnit("zettahenry", "ZH", Nil, r"1e21")
  final case object yottahenry extends DefaultInductanceUnit("yottahenry", "YH", Nil, r"1e24")
}

object InductanceUnits{
  def H: InductanceUnit = InductanceUnitObjects.henry
  def yH: InductanceUnit = InductanceUnitObjects.yoctohenry
  def zH: InductanceUnit = InductanceUnitObjects.zeptohenry
  def aH: InductanceUnit = InductanceUnitObjects.attohenry
  def fH: InductanceUnit = InductanceUnitObjects.femtohenry
  def pH: InductanceUnit = InductanceUnitObjects.picohenry
  def nH: InductanceUnit = InductanceUnitObjects.nanohenry
  def `μH`: InductanceUnit = InductanceUnitObjects.microhenry
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