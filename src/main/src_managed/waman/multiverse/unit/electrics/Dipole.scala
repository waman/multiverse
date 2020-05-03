package waman.multiverse.unit.electrics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


class Dipole[A: Fractional](val value: A, val unit: DipoleUnit)
    extends LinearQuantity[Dipole[A], A, DipoleUnit] {

  override protected def newQuantity(value: A, unit: DipoleUnit): Dipole[A] = new Dipole(value, unit)
}

trait DipoleUnit extends LinearUnit[DipoleUnit]{

  override def getSIUnit: DipoleUnit = DipoleUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = DipoleUnit.dimension
}

object DipoleUnit extends UnitInfo[DipoleUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, I -> 1, L -> 1).withDefaultValue(0)

  import waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: DipoleUnit = ChargeUnit.getSIUnit * LengthUnit.getSIUnit

  import DipoleUnitObjects._
  def getUnits: Seq[DipoleUnit] =
    Seq(debye, atomic_unit_of_electric_dipole_moment)
}

/** For no aliase or user defined units */
class SimpleDipoleUnit(val name: String, val symbol: String, val interval: Real) extends DipoleUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultDipoleUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends DipoleUnit

object DipoleUnitObjects{

  import spire.implicits._

  import waman.multiverse.unit.Constants
  import waman.multiverse.unit.electrics.ChargeUnitObjects._

  final case object debye extends SimpleDipoleUnit("debye", "D", r"1e-20" * statcoulomb.interval)
  final case object atomic_unit_of_electric_dipole_moment extends SimpleDipoleUnit("atomic unit of electric dipole moment", "ea_0", Constants.BohrRadius) with NotExact
}

object DipoleUnits{

  def D: DipoleUnit = DipoleUnitObjects.debye
  def ea_0: DipoleUnit = DipoleUnitObjects.atomic_unit_of_electric_dipole_moment
}