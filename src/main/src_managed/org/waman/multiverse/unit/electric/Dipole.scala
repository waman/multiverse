package org.waman.multiverse.unit.electric

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Dipole[A: Fractional](val value: A, val unit: DipoleUnit)
    extends LinearQuantity[Dipole[A], A, DipoleUnit] {

  override protected def newQuantity(value: A, unit: DipoleUnit): Dipole[A] = new Dipole(value, unit)

}

trait DipoleUnit extends LinearUnit[DipoleUnit]{

  override def getSIUnit: DipoleUnit = DipoleUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = DipoleUnit.dimension

}

object DipoleUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, I -> 1, L -> 1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: DipoleUnit = ChargeUnit.getSIUnit * LengthUnit.getSIUnit

  import DipoleUnitObjects._
  def getUnits: Seq[DipoleUnit] =
    Seq(debye, atomic_unit_of_electric_dipole_moment)
}

class DefaultDipoleUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends DipoleUnit

object DipoleUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object debye extends DefaultDipoleUnit("debye", "D", Nil, r"1e-20" * ChargeUnitObjects.statcoulomb.interval)
  final object atomic_unit_of_electric_dipole_moment extends DefaultDipoleUnit("atomic unit of electric dipole moment", "ea_0", Nil, Constants.BohrRadius) with NotExact
}

object DipoleUnits{
  def D: DipoleUnit = DipoleUnitObjects.debye
  def ea_0: DipoleUnit = DipoleUnitObjects.atomic_unit_of_electric_dipole_moment
}