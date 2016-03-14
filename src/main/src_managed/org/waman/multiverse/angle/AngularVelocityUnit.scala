package org.waman.multiverse.angle

import org.waman.multiverse.MultiverseUtil.twoPi
import org.waman.multiverse._
import org.waman.multiverse.time.TimeUnit
import spire.implicits._
import spire.math.Real

sealed trait AngularVelocityUnit extends PhysicalUnit[AngularVelocityUnit]{

  def unitInRadianPerSecond: Real

  override def baseUnit = AngleUnit.Radian / TimeUnit.Second
  override def valueInBaseUnit = unitInRadianPerSecond
}

object AngularVelocityUnit extends ConstantsDefined[AngularVelocityUnit]{

  // intrinsic
  private[AngularVelocityUnit]
  class IntrinsicAngularVelocityUnit(name: String, val symbols: Seq[String], val unitInRadianPerSecond: Real)
      extends AngularVelocityUnit{

    def this(name: String, symbols: Seq[String], unit: AngularVelocityUnit) =
      this(name, symbols, unit.unitInRadianPerSecond)

    def this(name: String, symbols: Seq[String], factor: Real, unit: AngularVelocityUnit) =
      this(name, symbols, factor * unit.unitInRadianPerSecond)
  }

  case object CyclePerSecond extends IntrinsicAngularVelocityUnit("CyclePerSecond", Seq("cps"), twoPi)
    
  case object RevolutionPerMinute extends IntrinsicAngularVelocityUnit("RevolutionPerMinute", Seq("rpm"), twoPi / r"60")
    

  override lazy val values = Seq(CyclePerSecond, RevolutionPerMinute)

  // AngleUnit / TimeUnit -> AngularVelocity
  private[AngularVelocityUnit]
  class AnglePerTimeUnit(val numeratorUnit: AngleUnit, val denominatorUnit: TimeUnit)
      extends AngularVelocityUnit with QuotientUnit[AngularVelocityUnit, AngleUnit, TimeUnit]{

    override lazy val unitInRadianPerSecond: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: AngleUnit, dUnit: TimeUnit): AngularVelocityUnit =
    new AnglePerTimeUnit(nUnit, dUnit)
}

trait AngularVelocityPostfixOps[A]{
  import AngularVelocityUnit._

  protected def angularVelocityPostfixOps(unit: AngularVelocityUnit): A

  def cps : A = angularVelocityPostfixOps(CyclePerSecond)
  def rpm : A = angularVelocityPostfixOps(RevolutionPerMinute)
}

trait AngularVelocityDot[A]{
  import AngularVelocityUnit._

  protected def angularVelocityDot(unit: AngularVelocityUnit): A

  def cps(dot: Dot): A = angularVelocityDot(CyclePerSecond)
  def rpm(dot: Dot): A = angularVelocityDot(RevolutionPerMinute)
}

trait AngularVelocityPer[A]{
  import AngularVelocityUnit._

  protected def angularVelocityPer(unit: AngularVelocityUnit): A

  def cps(per: Per): A = angularVelocityPer(CyclePerSecond)
  def rpm(per: Per): A = angularVelocityPer(RevolutionPerMinute)
}

trait PredefinedAngularVelocityUnit extends AngularVelocityPostfixOps[AngularVelocityUnit]{
  override protected def angularVelocityPostfixOps(unit: AngularVelocityUnit) = unit
  
}

object PredefinedAngularVelocityUnit extends PredefinedAngularVelocityUnit
