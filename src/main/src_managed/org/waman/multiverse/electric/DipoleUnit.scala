package org.waman.multiverse.electric

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.electric.ChargeUnit._
import org.waman.multiverse.metric.LengthUnit.AtomicUnitOfLength

sealed trait DipoleUnit extends PhysicalUnit[DipoleUnit]{

  def unitInCoulombMetre: Real

  override def baseUnit = ChargeUnit.Coulomb * LengthUnit.Metre
  override def valueInBaseUnit = unitInCoulombMetre
}

object DipoleUnit extends ConstantsDefined[DipoleUnit]{

  // intrinsic
  private[DipoleUnit]
  class IntrinsicDipoleUnit(name: String, val symbols: Seq[String], val unitInCoulombMetre: Real)
      extends DipoleUnit{

    def this(name: String, symbols: Seq[String], unit: DipoleUnit) =
      this(name, symbols, unit.unitInCoulombMetre)

    def this(name: String, symbols: Seq[String], factor: Real, unit: DipoleUnit) =
      this(name, symbols, factor * unit.unitInCoulombMetre)
  }


  case object Debye extends IntrinsicDipoleUnit("Debye", Seq("D"), r"1e-20" * Statcoulomb.unitInCoulomb)
  case object AtomicUnitOfElectricDipoleMoment extends IntrinsicDipoleUnit("AtomicUnitOfElectricDipoleMoment", Seq("ea0"), ElementaryCharge.unitInCoulomb * AtomicUnitOfLength.unitInMetre)

  override lazy val values = Seq(Debye, AtomicUnitOfElectricDipoleMoment)

  // ChargeUnit * LengthUnit -> Dipole
  private[DipoleUnit]
  class ProductChargeDotLengthUnit(val firstUnit: ChargeUnit, val secondUnit: LengthUnit)
      extends DipoleUnit with ProductUnit[DipoleUnit, ChargeUnit, LengthUnit]{

    override lazy val unitInCoulombMetre: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: ChargeUnit, unit2: LengthUnit): DipoleUnit =
    new ProductChargeDotLengthUnit(unit1, unit2)
}

trait MultiplicativeByDipoleUnit[R]{
  def *(unit: DipoleUnit): R
}

trait DivisibleByDipoleUnit[R]{
  def /(unit: DipoleUnit): R
}

trait DipolePostfixOps[A]{
  import DipoleUnit._

  protected def dipolePostfixOps(unit: DipoleUnit): A


  def D : A = dipolePostfixOps(Debye)
  def ea0 : A = dipolePostfixOps(AtomicUnitOfElectricDipoleMoment)
}

trait DipoleDot[A]{
  import DipoleUnit._

  protected def dipoleDot(unit: DipoleUnit): A

  def D(dot: Dot): A = dipoleDot(Debye)
  def ea0(dot: Dot): A = dipoleDot(AtomicUnitOfElectricDipoleMoment)
}

trait DipolePer[A]{
  import DipoleUnit._

  protected def dipolePer(unit: DipoleUnit): A

  def D(per: Per): A = dipolePer(Debye)
  def ea0(per: Per): A = dipolePer(AtomicUnitOfElectricDipoleMoment)
}

trait PredefinedDipoleUnit extends DipolePostfixOps[DipoleUnit]{
  override protected def dipolePostfixOps(unit: DipoleUnit) = unit
  
}

object PredefinedDipoleUnit extends PredefinedDipoleUnit
