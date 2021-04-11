package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.Constants

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
    Seq(henry, yoctohenry, zeptohenry, attohenry, femtohenry, picohenry, nanohenry, microhenry, millihenry, centihenry, decihenry, decahenry, hectohenry, kilohenry, megahenry, gigahenry, terahenry, petahenry, exahenry, zettahenry, yottahenry, abhenry, stathenry)
}


/** For no aliase or user defined units */
class SimpleInductanceUnit(val name: String, val symbol: String, val interval: Real) extends InductanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultInductanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends InductanceUnit
  
object InductanceUnitObjects{

  final case object henry extends SimpleInductanceUnit("henry", "H", 1)
  final case object yoctohenry extends SimpleInductanceUnit("yoctohenry", "yH", r"1e-24")
  final case object zeptohenry extends SimpleInductanceUnit("zeptohenry", "zH", r"1e-21")
  final case object attohenry extends SimpleInductanceUnit("attohenry", "aH", r"1e-18")
  final case object femtohenry extends SimpleInductanceUnit("femtohenry", "fH", r"1e-15")
  final case object picohenry extends SimpleInductanceUnit("picohenry", "pH", r"1e-12")
  final case object nanohenry extends SimpleInductanceUnit("nanohenry", "nH", r"1e-9")
  final case object microhenry extends DefaultInductanceUnit("microhenry", "μH", Seq("mcH"), r"1e-6")
  final case object millihenry extends SimpleInductanceUnit("millihenry", "mH", r"1e-3")
  final case object centihenry extends SimpleInductanceUnit("centihenry", "cH", r"1e-2")
  final case object decihenry extends SimpleInductanceUnit("decihenry", "dH", r"1e-1")
  final case object decahenry extends SimpleInductanceUnit("decahenry", "daH", r"1e1")
  final case object hectohenry extends SimpleInductanceUnit("hectohenry", "hH", r"1e2")
  final case object kilohenry extends DefaultInductanceUnit("kilohenry", "kH", Seq("KH"), r"1e3")
  final case object megahenry extends SimpleInductanceUnit("megahenry", "MH", r"1e6")
  final case object gigahenry extends SimpleInductanceUnit("gigahenry", "GH", r"1e9")
  final case object terahenry extends SimpleInductanceUnit("terahenry", "TH", r"1e12")
  final case object petahenry extends SimpleInductanceUnit("petahenry", "PH", r"1e15")
  final case object exahenry extends SimpleInductanceUnit("exahenry", "EH", r"1e18")
  final case object zettahenry extends SimpleInductanceUnit("zettahenry", "ZH", r"1e21")
  final case object yottahenry extends SimpleInductanceUnit("yottahenry", "YH", r"1e24")
  final case object abhenry extends SimpleInductanceUnit("abhenry", "abH", r"1e-9")
  final case object stathenry extends SimpleInductanceUnit("stathenry", "statH", Constants.SpeedOfLight * Constants.SpeedOfLight * r"1e-5") with Description {
    def description: String = "Formal unit for Gaussian and ESU CGS unit system."
  }
}


object InductanceUnits{

  /** henry */
  def H: InductanceUnit = InductanceUnitObjects.henry
  /** yoctohenry */
  def yH: InductanceUnit = InductanceUnitObjects.yoctohenry
  /** zeptohenry */
  def zH: InductanceUnit = InductanceUnitObjects.zeptohenry
  /** attohenry */
  def aH: InductanceUnit = InductanceUnitObjects.attohenry
  /** femtohenry */
  def fH: InductanceUnit = InductanceUnitObjects.femtohenry
  /** picohenry */
  def pH: InductanceUnit = InductanceUnitObjects.picohenry
  /** nanohenry */
  def nH: InductanceUnit = InductanceUnitObjects.nanohenry
  /** microhenry */
  def μH: InductanceUnit = InductanceUnitObjects.microhenry
  /** microhenry */
  def mcH: InductanceUnit = InductanceUnitObjects.microhenry
  /** millihenry */
  def mH: InductanceUnit = InductanceUnitObjects.millihenry
  /** centihenry */
  def cH: InductanceUnit = InductanceUnitObjects.centihenry
  /** decihenry */
  def dH: InductanceUnit = InductanceUnitObjects.decihenry
  /** decahenry */
  def daH: InductanceUnit = InductanceUnitObjects.decahenry
  /** hectohenry */
  def hH: InductanceUnit = InductanceUnitObjects.hectohenry
  /** kilohenry */
  def kH: InductanceUnit = InductanceUnitObjects.kilohenry
  /** kilohenry */
  def KH: InductanceUnit = InductanceUnitObjects.kilohenry
  /** megahenry */
  def MH: InductanceUnit = InductanceUnitObjects.megahenry
  /** gigahenry */
  def GH: InductanceUnit = InductanceUnitObjects.gigahenry
  /** terahenry */
  def TH: InductanceUnit = InductanceUnitObjects.terahenry
  /** petahenry */
  def PH: InductanceUnit = InductanceUnitObjects.petahenry
  /** exahenry */
  def EH: InductanceUnit = InductanceUnitObjects.exahenry
  /** zettahenry */
  def ZH: InductanceUnit = InductanceUnitObjects.zettahenry
  /** yottahenry */
  def YH: InductanceUnit = InductanceUnitObjects.yottahenry
  /** abhenry */
  def abH: InductanceUnit = InductanceUnitObjects.abhenry
  /** stathenry */
  def statH: InductanceUnit = InductanceUnitObjects.stathenry
}