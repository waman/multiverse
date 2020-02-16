package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
    extends LinearQuantity[Acceleration[A], A, AccelerationUnit] {

  override protected def newQuantity(value: A, unit: AccelerationUnit): Acceleration[A] = new Acceleration(value, unit)
}

/** null */
trait AccelerationUnit extends LinearUnit[AccelerationUnit]{

  override def getSIUnit: AccelerationUnit = AccelerationUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AccelerationUnit.dimension
}

object AccelerationUnit extends UnitInfo[AccelerationUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.LengthUnit
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

  final case object standard_gravity extends DefaultAccelerationUnit("standard gravity", "G", Seq("g_0"), r"9.80665")
  final case object gal extends SimpleAccelerationUnit("gal", "Gal", r"1e-2")
}

object AccelerationUnits{
  def G: AccelerationUnit = AccelerationUnitObjects.standard_gravity
  def g_0: AccelerationUnit = AccelerationUnitObjects.standard_gravity
  def Gal: AccelerationUnit = AccelerationUnitObjects.gal
}