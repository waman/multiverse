package waman.multiverse.typeless

import spire.math.Fractional
import waman.multiverse.{LinearQuantity, Quantity}

class TypelessLinearQuantity[A: Fractional](val value: A, val unit: TypelessLinearUnit)
  extends LinearQuantity[TypelessLinearQuantity[A], A, TypelessLinearUnit] {

  override protected def newQuantity(value: A, unit: TypelessLinearUnit): TypelessLinearQuantity[A] =
    new TypelessLinearQuantity[A](value, unit)

  private def checkUnitDimension(evalUnit: TypelessLinearUnit): Unit =
    if(evalUnit.dimension != this.unit.dimension)
      throw new IllegalArgumentException(
        s"""The dimension of the argument unit is illegal:
           |  expected unit dimension: ${this.unit.dimension}
           |  appeared unit dimension:   ${evalUnit.dimension}""".stripMargin)

  override protected def applyInDifferentUnit(evalUnit: TypelessLinearUnit): A = {
    checkUnitDimension(evalUnit)
    super.applyInDifferentUnit(evalUnit)
  }

  override protected def compareInDifferentUnit(that: Quantity[A, TypelessLinearUnit]): Int = {
    checkUnitDimension(that.unit)
    super.compareInDifferentUnit(that)
  }

  override def +(that: TypelessLinearQuantity[A]): TypelessLinearQuantity[A] = {
    checkUnitDimension(that.unit)
    super.+(that)
  }

  /** harmonic sum: v1v2/(v1 + v2) or v of 1/v =  1/v1 + 1/v2 */
  override def |+|(that: TypelessLinearQuantity[A]): TypelessLinearQuantity[A] = {
    checkUnitDimension(that.unit)
    super.|+|(that)
  }

  override def -(that: TypelessLinearQuantity[A]): TypelessLinearQuantity[A] = {
    checkUnitDimension(that.unit)
    super.-(that)
  }
}