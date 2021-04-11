package org.waman.multiverse.unit.defs.mechanics

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
    extends LinearQuantity[Acceleration[A], A, AccelerationUnit] {

  override protected def newQuantity(value: A, unit: AccelerationUnit): Acceleration[A] = new Acceleration(value, unit)

  def *(mass: Mass[A]): Force[A] = new Force(this.value * mass.value, mass.unit * this.unit)
}

trait AccelerationUnit extends LinearUnit[AccelerationUnit]{

  override def getSIUnit: AccelerationUnit = AccelerationUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AccelerationUnit.dimension
}

object AccelerationUnit extends UnitInfo[AccelerationUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 1).withDefaultValue(0)

  val getSIUnit: AccelerationUnit = LengthUnit.getSIUnit / TimeSquaredUnit.getSIUnit
  import AccelerationUnitObjects._

  def getUnits: Seq[AccelerationUnit] =
    Seq(standard_gravity, gal)
}


/** For no aliase or user defined units */
class SimpleAccelerationUnit(val name: String, val symbol: String, val interval: Real) extends AccelerationUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAccelerationUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AccelerationUnit
  
object AccelerationUnitObjects{

  final case object standard_gravity extends DefaultAccelerationUnit("standard gravity", "G", Seq("g0"), r"9.80665")
  final case object gal extends SimpleAccelerationUnit("gal", "Gal", r"1e-2")
}


object AccelerationUnits{

  /** standard gravity */
  def G: AccelerationUnit = AccelerationUnitObjects.standard_gravity
  /** standard gravity */
  def g0: AccelerationUnit = AccelerationUnitObjects.standard_gravity
  /** gal */
  def Gal: AccelerationUnit = AccelerationUnitObjects.gal
}