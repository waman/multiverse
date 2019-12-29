package org.waman.multiverse.unit.chemistry

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class AmountOfSubstance[A: Fractional](val value: A, val unit: AmountOfSubstanceUnit)
    extends LinearQuantity[AmountOfSubstance[A], A, AmountOfSubstanceUnit] {

  override protected def newQuantity(value: A, unit: AmountOfSubstanceUnit): AmountOfSubstance[A] = new AmountOfSubstance(value, unit)

}

trait AmountOfSubstanceUnit extends LinearUnit[AmountOfSubstanceUnit]{

  override def getSIUnit: AmountOfSubstanceUnit = AmountOfSubstanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AmountOfSubstanceUnit.dimension

}

object AmountOfSubstanceUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](N -> 1).withDefaultValue(0)

  def getSIUnit: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.mole

  import AmountOfSubstanceUnitObjects._
  def getUnits: Seq[AmountOfSubstanceUnit] =
    Seq(mole)
}

class DefaultAmountOfSubstanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AmountOfSubstanceUnit

object AmountOfSubstanceUnitObjects{

  final object mole extends DefaultAmountOfSubstanceUnit("mole", "mol", Nil, 1)
}

object AmountOfSubstanceUnits{
  def mol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.mole
}