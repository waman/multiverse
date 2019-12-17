package org.waman.multiverse.unit.fluid

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class KinematicViscosity[A: Fractional](val value: A, val unit: KinematicViscosityUnit)
    extends LinearQuantity[KinematicViscosity[A], A, KinematicViscosityUnit] {

  override protected def newQuantity(value: A, unit: KinematicViscosityUnit): KinematicViscosity[A] = new KinematicViscosity(value, unit)
}

trait KinematicViscosityUnit extends LinearUnit[KinematicViscosityUnit]{

  override def getSIUnit: KinematicViscosityUnit = KinematicViscosityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = KinematicViscosityUnit.dimension

}

object KinematicViscosityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.AreaUnit
  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: KinematicViscosityUnit = AreaUnit.getSIUnit / TimeUnit.getSIUnit

  import KinematicViscosityUnitObjects._
  def getUnits: Seq[KinematicViscosityUnit] =
    Seq(stokes)
}



class DefaultKinematicViscosityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends KinematicViscosityUnit

object KinematicViscosityUnitObjects{

  final object stokes extends DefaultKinematicViscosityUnit("stokes", "St", Nil, r"1e-4")
}

object KinematicViscosityUnits{
  def St: KinematicViscosityUnit = KinematicViscosityUnitObjects.stokes
}