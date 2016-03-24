package org.waman.multiverse.mechanics

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric.LengthUnit
import org.waman.multiverse.time.TimeUnit
import org.waman.multiverse.time.TimeSquaredUnit

sealed trait AccelerationUnit extends PhysicalUnit[AccelerationUnit]{

  def unitInMetrePerSecondSquared: Real

  override def baseUnit = LengthUnit.Metre / TimeSquaredUnit.SecondSquared
  override def valueInBaseUnit = unitInMetrePerSecondSquared
}

object AccelerationUnit extends ConstantsDefined[AccelerationUnit]{

  // intrinsic
  private[AccelerationUnit]
  class IntrinsicAccelerationUnit(name: String, val symbols: Seq[String], val unitInMetrePerSecondSquared: Real)
      extends AccelerationUnit{

    def this(name: String, symbols: Seq[String], unit: AccelerationUnit) =
      this(name, symbols, unit.unitInMetrePerSecondSquared)

    def this(name: String, symbols: Seq[String], factor: Real, unit: AccelerationUnit) =
      this(name, symbols, factor * unit.unitInMetrePerSecondSquared)
  }


  case object StandardGravity extends IntrinsicAccelerationUnit("StandardGravity", Seq("g0"), r"9.80665")
  case object Galileo extends IntrinsicAccelerationUnit("Galileo", Seq("Gal"), LengthUnit.CentiMetre / TimeSquaredUnit.SecondSquared)
  case object InchPerSecondSquared extends IntrinsicAccelerationUnit("InchPerSecondSquared", Seq("ips2"), LengthUnit.Inch / TimeSquaredUnit.SecondSquared)
  case object FootPerSecondSquared extends IntrinsicAccelerationUnit("FootPerSecondSquared", Seq("fps2"), LengthUnit.Foot / TimeSquaredUnit.SecondSquared)
  case object MilePerSecondSquared extends IntrinsicAccelerationUnit("MilePerSecondSquared", Seq("mps2"), LengthUnit.Mile / TimeSquaredUnit.SecondSquared)

  override lazy val values = Seq(StandardGravity, Galileo, InchPerSecondSquared, FootPerSecondSquared, MilePerSecondSquared)

  // LengthUnit / TimeSquaredUnit -> Acceleration
  private[AccelerationUnit]
  class QuotientLengthPerTimeSquaredUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeSquaredUnit)
      extends AccelerationUnit with QuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit]{

    override lazy val unitInMetrePerSecondSquared: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: LengthUnit, dUnit: TimeSquaredUnit): AccelerationUnit =
    new QuotientLengthPerTimeSquaredUnit(nUnit, dUnit)

  // VelocityUnit / TimeUnit -> Acceleration
  private[AccelerationUnit]
  class QuotientVelocityPerTimeUnit(val numeratorUnit: VelocityUnit, val denominatorUnit: TimeUnit)
      extends AccelerationUnit with QuotientUnit[AccelerationUnit, VelocityUnit, TimeUnit]{

    override lazy val unitInMetrePerSecondSquared: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: VelocityUnit, dUnit: TimeUnit): AccelerationUnit =
    new QuotientVelocityPerTimeUnit(nUnit, dUnit)
}

trait MultiplicativeByAccelerationUnit[R]{
  def *(unit: AccelerationUnit): R
}

trait DivisibleByAccelerationUnit[R]{
  def /(unit: AccelerationUnit): R
}

trait AccelerationPostfixOps[A]{
  import AccelerationUnit._

  protected def accelerationPostfixOps(unit: AccelerationUnit): A


  def g0 : A = accelerationPostfixOps(StandardGravity)
  def Gal : A = accelerationPostfixOps(Galileo)
  def ips2 : A = accelerationPostfixOps(InchPerSecondSquared)
  def fps2 : A = accelerationPostfixOps(FootPerSecondSquared)
  def mps2 : A = accelerationPostfixOps(MilePerSecondSquared)
}

trait AccelerationDot[A]{
  import AccelerationUnit._

  protected def accelerationDot(unit: AccelerationUnit): A

  def g0(dot: Dot): A = accelerationDot(StandardGravity)
  def Gal(dot: Dot): A = accelerationDot(Galileo)
  def ips2(dot: Dot): A = accelerationDot(InchPerSecondSquared)
  def fps2(dot: Dot): A = accelerationDot(FootPerSecondSquared)
  def mps2(dot: Dot): A = accelerationDot(MilePerSecondSquared)
}

trait AccelerationPer[A]{
  import AccelerationUnit._

  protected def accelerationPer(unit: AccelerationUnit): A

  def g0(per: Per): A = accelerationPer(StandardGravity)
  def Gal(per: Per): A = accelerationPer(Galileo)
  def ips2(per: Per): A = accelerationPer(InchPerSecondSquared)
  def fps2(per: Per): A = accelerationPer(FootPerSecondSquared)
  def mps2(per: Per): A = accelerationPer(MilePerSecondSquared)
}

trait PredefinedAccelerationUnit extends AccelerationPostfixOps[AccelerationUnit]{
  override protected def accelerationPostfixOps(unit: AccelerationUnit) = unit
  
}

object PredefinedAccelerationUnit extends PredefinedAccelerationUnit
