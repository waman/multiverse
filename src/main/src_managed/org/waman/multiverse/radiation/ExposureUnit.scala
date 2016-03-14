package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.electric.ChargeUnit
import org.waman.multiverse.mass.MassUnit
import spire.implicits._
import spire.math.Real

sealed trait ExposureUnit extends PhysicalUnit[ExposureUnit]{

  def unitInCoulombPerKiloGram: Real

  override def baseUnit = ChargeUnit.Coulomb / MassUnit.KiloGram
  override def valueInBaseUnit = unitInCoulombPerKiloGram
}

object ExposureUnit extends ConstantsDefined[ExposureUnit]{

  // intrinsic
  private[ExposureUnit]
  class IntrinsicExposureUnit(name: String, val symbols: Seq[String], val unitInCoulombPerKiloGram: Real)
      extends ExposureUnit{

    def this(name: String, symbols: Seq[String], unit: ExposureUnit) =
      this(name, symbols, unit.unitInCoulombPerKiloGram)

    def this(name: String, symbols: Seq[String], factor: Real, unit: ExposureUnit) =
      this(name, symbols, factor * unit.unitInCoulombPerKiloGram)
  }

  case object Roentgen extends IntrinsicExposureUnit("Roentgen", Seq("R"), r"2.58e-4")
    

  override lazy val values = Seq(Roentgen)

  // ChargeUnit / MassUnit -> Exposure
  private[ExposureUnit]
  class ChargePerMassUnit(val numeratorUnit: ChargeUnit, val denominatorUnit: MassUnit)
      extends ExposureUnit with QuotientUnit[ExposureUnit, ChargeUnit, MassUnit]{

    override lazy val unitInCoulombPerKiloGram: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: ChargeUnit, dUnit: MassUnit): ExposureUnit =
    new ChargePerMassUnit(nUnit, dUnit)
}

trait ExposurePostfixOps[A]{
  import ExposureUnit._

  protected def exposurePostfixOps(unit: ExposureUnit): A

  def R : A = exposurePostfixOps(Roentgen)
}

trait ExposureDot[A]{
  import ExposureUnit._

  protected def exposureDot(unit: ExposureUnit): A

  def R(dot: Dot): A = exposureDot(Roentgen)
}

trait ExposurePer[A]{
  import ExposureUnit._

  protected def exposurePer(unit: ExposureUnit): A

  def R(per: Per): A = exposurePer(Roentgen)
}

trait PredefinedExposureUnit extends ExposurePostfixOps[ExposureUnit]{
  override protected def exposurePostfixOps(unit: ExposureUnit) = unit
  
}

object PredefinedExposureUnit extends PredefinedExposureUnit
