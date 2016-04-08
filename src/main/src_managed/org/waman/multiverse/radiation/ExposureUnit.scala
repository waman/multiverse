package org.waman.multiverse.radiation

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.electric.ChargeUnit
import org.waman.multiverse.mass.MassUnit

sealed trait ExposureUnit extends PhysicalUnit[ExposureUnit]{

  override def getSIUnit = ChargeUnit.Coulomb / MassUnit.KiloGram
}

object ExposureUnit extends ConstantsDefined[ExposureUnit]{

  // intrinsic
  private[ExposureUnit]
  class IntrinsicExposureUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends ExposureUnit{

    def this(name: String, symbols: Seq[String], unit: ExposureUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: ExposureUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object Roentgen extends IntrinsicExposureUnit("Roentgen", Seq("R"), r"2.58e-4")

  override lazy val values = Seq(Roentgen)

  // ChargeUnit / MassUnit -> Exposure
  private[ExposureUnit]
  class QuotientChargePerMassUnit(val numeratorUnit: ChargeUnit, val denominatorUnit: MassUnit)
      extends ExposureUnit with QuotientUnit[ExposureUnit, ChargeUnit, MassUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: ChargeUnit, dUnit: MassUnit): ExposureUnit =
    new QuotientChargePerMassUnit(nUnit, dUnit)
}

trait MultiplicativeByExposureUnit[R]{
  def *(unit: ExposureUnit): R
}

trait DivisibleByExposureUnit[R]{
  def /(unit: ExposureUnit): R
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
