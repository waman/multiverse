package org.waman.multiverse.unit.defs.fluid

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class DynamicViscosity[A: Fractional](val value: A, val unit: DynamicViscosityUnit)
    extends LinearQuantity[DynamicViscosity[A], A, DynamicViscosityUnit] {

  override protected def newQuantity(value: A, unit: DynamicViscosityUnit): DynamicViscosity[A] = new DynamicViscosity(value, unit)
}

/** None */
trait DynamicViscosityUnit extends LinearUnit[DynamicViscosityUnit]{

  override def getSIUnit: DynamicViscosityUnit = DynamicViscosityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = DynamicViscosityUnit.dimension
}

object DynamicViscosityUnit extends UnitInfo[DynamicViscosityUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, M -> 1, L -> -1).withDefaultValue(0)

  val getSIUnit: DynamicViscosityUnit = PressureUnit.getSIUnit * TimeUnit.getSIUnit

  import DynamicViscosityUnitObjects._

  def getUnits: Seq[DynamicViscosityUnit] =
    Seq(poise, yoctopoise, zeptopoise, attopoise, femtopoise, picopoise, nanopoise, micropoise, millipoise, centipoise, decipoise, decapoise, hectopoise, kilopoise, megapoise, gigapoise, terapoise, petapoise, exapoise, zettapoise, yottapoise)
}


/** For no alias or user defined units */
class SimpleDynamicViscosityUnit(val name: String, val symbol: String, val interval: Real) extends DynamicViscosityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultDynamicViscosityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends DynamicViscosityUnit
  
object DynamicViscosityUnitObjects{

  final case object poise extends SimpleDynamicViscosityUnit("poise", "P", r"0.1")
  final case object yoctopoise extends SimpleDynamicViscosityUnit("yoctopoise", "yP", r"0.1" * r"1e-24")
  final case object zeptopoise extends SimpleDynamicViscosityUnit("zeptopoise", "zP", r"0.1" * r"1e-21")
  final case object attopoise extends SimpleDynamicViscosityUnit("attopoise", "aP", r"0.1" * r"1e-18")
  final case object femtopoise extends SimpleDynamicViscosityUnit("femtopoise", "fP", r"0.1" * r"1e-15")
  final case object picopoise extends SimpleDynamicViscosityUnit("picopoise", "pP", r"0.1" * r"1e-12")
  final case object nanopoise extends SimpleDynamicViscosityUnit("nanopoise", "nP", r"0.1" * r"1e-9")
  final case object micropoise extends DefaultDynamicViscosityUnit("micropoise", "μP", Seq("mcP"), r"0.1" * r"1e-6")
  final case object millipoise extends SimpleDynamicViscosityUnit("millipoise", "mP", r"0.1" * r"1e-3")
  final case object centipoise extends SimpleDynamicViscosityUnit("centipoise", "cP", r"0.1" * r"1e-2")
  final case object decipoise extends SimpleDynamicViscosityUnit("decipoise", "dP", r"0.1" * r"1e-1")
  final case object decapoise extends SimpleDynamicViscosityUnit("decapoise", "daP", r"0.1" * r"1e1")
  final case object hectopoise extends SimpleDynamicViscosityUnit("hectopoise", "hP", r"0.1" * r"1e2")
  final case object kilopoise extends DefaultDynamicViscosityUnit("kilopoise", "kP", Seq("KP"), r"0.1" * r"1e3")
  final case object megapoise extends SimpleDynamicViscosityUnit("megapoise", "MP", r"0.1" * r"1e6")
  final case object gigapoise extends SimpleDynamicViscosityUnit("gigapoise", "GP", r"0.1" * r"1e9")
  final case object terapoise extends SimpleDynamicViscosityUnit("terapoise", "TP", r"0.1" * r"1e12")
  final case object petapoise extends SimpleDynamicViscosityUnit("petapoise", "PP", r"0.1" * r"1e15")
  final case object exapoise extends SimpleDynamicViscosityUnit("exapoise", "EP", r"0.1" * r"1e18")
  final case object zettapoise extends SimpleDynamicViscosityUnit("zettapoise", "ZP", r"0.1" * r"1e21")
  final case object yottapoise extends SimpleDynamicViscosityUnit("yottapoise", "YP", r"0.1" * r"1e24")
}


object DynamicViscosityUnits{

  /** poise */
  def P: DynamicViscosityUnit = DynamicViscosityUnitObjects.poise
  /** yoctopoise */
  def yP: DynamicViscosityUnit = DynamicViscosityUnitObjects.yoctopoise
  /** zeptopoise */
  def zP: DynamicViscosityUnit = DynamicViscosityUnitObjects.zeptopoise
  /** attopoise */
  def aP: DynamicViscosityUnit = DynamicViscosityUnitObjects.attopoise
  /** femtopoise */
  def fP: DynamicViscosityUnit = DynamicViscosityUnitObjects.femtopoise
  /** picopoise */
  def pP: DynamicViscosityUnit = DynamicViscosityUnitObjects.picopoise
  /** nanopoise */
  def nP: DynamicViscosityUnit = DynamicViscosityUnitObjects.nanopoise
  /** micropoise */
  def μP: DynamicViscosityUnit = DynamicViscosityUnitObjects.micropoise
  /** micropoise */
  def mcP: DynamicViscosityUnit = DynamicViscosityUnitObjects.micropoise
  /** millipoise */
  def mP: DynamicViscosityUnit = DynamicViscosityUnitObjects.millipoise
  /** centipoise */
  def cP: DynamicViscosityUnit = DynamicViscosityUnitObjects.centipoise
  /** decipoise */
  def dP: DynamicViscosityUnit = DynamicViscosityUnitObjects.decipoise
  /** decapoise */
  def daP: DynamicViscosityUnit = DynamicViscosityUnitObjects.decapoise
  /** hectopoise */
  def hP: DynamicViscosityUnit = DynamicViscosityUnitObjects.hectopoise
  /** kilopoise */
  def kP: DynamicViscosityUnit = DynamicViscosityUnitObjects.kilopoise
  /** kilopoise */
  def KP: DynamicViscosityUnit = DynamicViscosityUnitObjects.kilopoise
  /** megapoise */
  def MP: DynamicViscosityUnit = DynamicViscosityUnitObjects.megapoise
  /** gigapoise */
  def GP: DynamicViscosityUnit = DynamicViscosityUnitObjects.gigapoise
  /** terapoise */
  def TP: DynamicViscosityUnit = DynamicViscosityUnitObjects.terapoise
  /** petapoise */
  def PP: DynamicViscosityUnit = DynamicViscosityUnitObjects.petapoise
  /** exapoise */
  def EP: DynamicViscosityUnit = DynamicViscosityUnitObjects.exapoise
  /** zettapoise */
  def ZP: DynamicViscosityUnit = DynamicViscosityUnitObjects.zettapoise
  /** yottapoise */
  def YP: DynamicViscosityUnit = DynamicViscosityUnitObjects.yottapoise
}