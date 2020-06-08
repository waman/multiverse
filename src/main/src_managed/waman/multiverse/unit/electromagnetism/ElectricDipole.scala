package waman.multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


class ElectricDipole[A: Fractional](val value: A, val unit: ElectricDipoleUnit)
    extends LinearQuantity[ElectricDipole[A], A, ElectricDipoleUnit] {

  override protected def newQuantity(value: A, unit: ElectricDipoleUnit): ElectricDipole[A] = new ElectricDipole(value, unit)
}

trait ElectricDipoleUnit extends LinearUnit[ElectricDipoleUnit]{

  override def getSIUnit: ElectricDipoleUnit = ElectricDipoleUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ElectricDipoleUnit.dimension
}

object ElectricDipoleUnit extends UnitInfo[ElectricDipoleUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, I -> 1, L -> 1).withDefaultValue(0)

  import waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: ElectricDipoleUnit = ElectricChargeUnit.getSIUnit * LengthUnit.getSIUnit

  import ElectricDipoleUnitObjects._
  def getUnits: Seq[ElectricDipoleUnit] =
    Seq(debye, atomic_unit_of_electric_dipole_moment)
}

/** For no aliase or user defined units */
class SimpleElectricDipoleUnit(val name: String, val symbol: String, val interval: Real) extends ElectricDipoleUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultElectricDipoleUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ElectricDipoleUnit

object ElectricDipoleUnitObjects{

  import spire.implicits._

  import waman.multiverse.unit.Constants
  import waman.multiverse.unit.electromagnetism.ElectricChargeUnitObjects._

  final case object debye extends SimpleElectricDipoleUnit("debye", "D", r"1e-20" * statcoulomb.interval)
  final case object atomic_unit_of_electric_dipole_moment extends SimpleElectricDipoleUnit("atomic unit of electric dipole moment", "ea_0", Constants.ElementaryCharge * Constants.BohrRadius) with NotExact
}

object ElectricDipoleUnits{

  def D: ElectricDipoleUnit = ElectricDipoleUnitObjects.debye
  def ea_0: ElectricDipoleUnit = ElectricDipoleUnitObjects.atomic_unit_of_electric_dipole_moment
}