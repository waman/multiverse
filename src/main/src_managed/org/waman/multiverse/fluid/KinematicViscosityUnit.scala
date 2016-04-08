package org.waman.multiverse.fluid

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric.AreaUnit
import org.waman.multiverse.time.TimeUnit

sealed trait KinematicViscosityUnit extends PhysicalUnit[KinematicViscosityUnit]{

  override def getSIUnit = AreaUnit.SquareMetre / TimeUnit.Second
}

object KinematicViscosityUnit extends ConstantsDefined[KinematicViscosityUnit]{

  // intrinsic
  private[KinematicViscosityUnit]
  class IntrinsicKinematicViscosityUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends KinematicViscosityUnit{

    def this(name: String, symbols: Seq[String], unit: KinematicViscosityUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: KinematicViscosityUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object Stokes extends IntrinsicKinematicViscosityUnit("Stokes", Seq("St"), r"1e-4")

  override lazy val values = Seq(Stokes)

  // AreaUnit / TimeUnit -> KinematicViscosity
  private[KinematicViscosityUnit]
  class QuotientAreaPerTimeUnit(val numeratorUnit: AreaUnit, val denominatorUnit: TimeUnit)
      extends KinematicViscosityUnit with QuotientUnit[KinematicViscosityUnit, AreaUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: AreaUnit, dUnit: TimeUnit): KinematicViscosityUnit =
    new QuotientAreaPerTimeUnit(nUnit, dUnit)
}

trait MultiplicativeByKinematicViscosityUnit[R]{
  def *(unit: KinematicViscosityUnit): R
}

trait DivisibleByKinematicViscosityUnit[R]{
  def /(unit: KinematicViscosityUnit): R
}

trait KinematicViscosityPostfixOps[A]{
  import KinematicViscosityUnit._

  protected def kinematicViscosityPostfixOps(unit: KinematicViscosityUnit): A


  def St : A = kinematicViscosityPostfixOps(Stokes)
}

trait KinematicViscosityDot[A]{
  import KinematicViscosityUnit._

  protected def kinematicViscosityDot(unit: KinematicViscosityUnit): A

  def St(dot: Dot): A = kinematicViscosityDot(Stokes)
}

trait KinematicViscosityPer[A]{
  import KinematicViscosityUnit._

  protected def kinematicViscosityPer(unit: KinematicViscosityUnit): A

  def St(per: Per): A = kinematicViscosityPer(Stokes)
}

trait PredefinedKinematicViscosityUnit extends KinematicViscosityPostfixOps[KinematicViscosityUnit]{
  override protected def kinematicViscosityPostfixOps(unit: KinematicViscosityUnit) = unit
  
}

object PredefinedKinematicViscosityUnit extends PredefinedKinematicViscosityUnit
