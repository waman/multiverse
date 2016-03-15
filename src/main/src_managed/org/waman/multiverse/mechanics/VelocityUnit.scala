package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.LengthUnit
import org.waman.multiverse.time.TimeUnit
import spire.implicits._
import spire.math.Real

sealed trait VelocityUnit extends PhysicalUnit[VelocityUnit]
  with DivisibleByTimeUnit[AccelerationUnit]{

  def unitInMetrePerSecond: Real

  override def baseUnit = LengthUnit.Metre / TimeUnit.Second
  override def valueInBaseUnit = unitInMetrePerSecond

  override def /(unit: TimeUnit) = AccelerationUnit(this, unit)
}

object VelocityUnit extends ConstantsDefined[VelocityUnit]{

  // intrinsic
  private[VelocityUnit]
  class IntrinsicVelocityUnit(name: String, val symbols: Seq[String], val unitInMetrePerSecond: Real)
      extends VelocityUnit{

    def this(name: String, symbols: Seq[String], unit: VelocityUnit) =
      this(name, symbols, unit.unitInMetrePerSecond)

    def this(name: String, symbols: Seq[String], factor: Real, unit: VelocityUnit) =
      this(name, symbols, factor * unit.unitInMetrePerSecond)
  }


  case object MachNumber extends IntrinsicVelocityUnit("MachNumber", Seq("M"), r"340") with NotExact
  case object SpeedOfLight extends IntrinsicVelocityUnit("SpeedOfLight", Seq("c"), r"299792458")
  case object Knot extends IntrinsicVelocityUnit("Knot", Seq("kn"), LengthUnit.NauticalMile / TimeUnit.Hour)
  case object Knot_Admiralty extends IntrinsicVelocityUnit("Knot_Admiralty", Seq("kn(Adm)"), LengthUnit.NauticalMile_Admiralty / TimeUnit.Hour)
  case object InchPerSecond extends IntrinsicVelocityUnit("InchPerSecond", Seq("ips"), LengthUnit.Inch / TimeUnit.Second)
  case object InchPerMinute extends IntrinsicVelocityUnit("InchPerMinute", Seq("ipm"), LengthUnit.Inch / TimeUnit.Minute)
  case object InchPerHour extends IntrinsicVelocityUnit("InchPerHour", Seq("iph"), LengthUnit.Inch / TimeUnit.Hour)
  case object FootPerSecond extends IntrinsicVelocityUnit("FootPerSecond", Seq("fps"), LengthUnit.Foot / TimeUnit.Second)
  case object FootPerMinute extends IntrinsicVelocityUnit("FootPerMinute", Seq("fpm"), LengthUnit.Foot / TimeUnit.Minute)
  case object FootPerHour extends IntrinsicVelocityUnit("FootPerHour", Seq("fph"), LengthUnit.Foot / TimeUnit.Hour)
  case object MilePerSecond extends IntrinsicVelocityUnit("MilePerSecond", Seq("mps"), LengthUnit.Mile / TimeUnit.Second)
  case object MilePerMinute extends IntrinsicVelocityUnit("MilePerMinute", Seq("mpm"), LengthUnit.Mile / TimeUnit.Minute)
  case object MilePerHour extends IntrinsicVelocityUnit("MilePerHour", Seq("mph"), LengthUnit.Mile / TimeUnit.Hour)

  override lazy val values = Seq(MachNumber, SpeedOfLight, Knot, Knot_Admiralty, InchPerSecond, InchPerMinute, InchPerHour, FootPerSecond, FootPerMinute, FootPerHour, MilePerSecond, MilePerMinute, MilePerHour)

  // LengthUnit / TimeUnit -> Velocity
  private[VelocityUnit]
  class QuotientLengthPerTimeUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
      extends VelocityUnit with QuotientUnit[VelocityUnit, LengthUnit, TimeUnit]{

    override lazy val unitInMetrePerSecond: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: LengthUnit, dUnit: TimeUnit): VelocityUnit =
    new QuotientLengthPerTimeUnit(nUnit, dUnit)
}

trait VelocityPostfixOps[A]{
  import VelocityUnit._

  protected def velocityPostfixOps(unit: VelocityUnit): A

  def M : A = velocityPostfixOps(MachNumber)
  def c : A = velocityPostfixOps(SpeedOfLight)
  def kn : A = velocityPostfixOps(Knot)
  def ips : A = velocityPostfixOps(InchPerSecond)
  def ipm : A = velocityPostfixOps(InchPerMinute)
  def iph : A = velocityPostfixOps(InchPerHour)
  def fps : A = velocityPostfixOps(FootPerSecond)
  def fpm : A = velocityPostfixOps(FootPerMinute)
  def fph : A = velocityPostfixOps(FootPerHour)
  def mps : A = velocityPostfixOps(MilePerSecond)
  def mpm : A = velocityPostfixOps(MilePerMinute)
  def mph : A = velocityPostfixOps(MilePerHour)
  import VelocityPostfixOps._

  def kn(c: Context): A = velocityPostfixOps(_kn(c))
}

object VelocityPostfixOps{
  import VelocityUnit._
  import org.waman.multiverse.Context._


  lazy val _kn : PartialFunction[Context, VelocityUnit] = {
    case Admiralty => Knot_Admiralty
  }
}

trait VelocityDot[A]{
  import VelocityUnit._

  protected def velocityDot(unit: VelocityUnit): A

  def M(dot: Dot): A = velocityDot(MachNumber)
  def c(dot: Dot): A = velocityDot(SpeedOfLight)
  def kn(dot: Dot): A = velocityDot(Knot)
  def ips(dot: Dot): A = velocityDot(InchPerSecond)
  def ipm(dot: Dot): A = velocityDot(InchPerMinute)
  def iph(dot: Dot): A = velocityDot(InchPerHour)
  def fps(dot: Dot): A = velocityDot(FootPerSecond)
  def fpm(dot: Dot): A = velocityDot(FootPerMinute)
  def fph(dot: Dot): A = velocityDot(FootPerHour)
  def mps(dot: Dot): A = velocityDot(MilePerSecond)
  def mpm(dot: Dot): A = velocityDot(MilePerMinute)
  def mph(dot: Dot): A = velocityDot(MilePerHour)
}

trait VelocityPer[A]{
  import VelocityUnit._

  protected def velocityPer(unit: VelocityUnit): A

  def M(per: Per): A = velocityPer(MachNumber)
  def c(per: Per): A = velocityPer(SpeedOfLight)
  def kn(per: Per): A = velocityPer(Knot)
  def ips(per: Per): A = velocityPer(InchPerSecond)
  def ipm(per: Per): A = velocityPer(InchPerMinute)
  def iph(per: Per): A = velocityPer(InchPerHour)
  def fps(per: Per): A = velocityPer(FootPerSecond)
  def fpm(per: Per): A = velocityPer(FootPerMinute)
  def fph(per: Per): A = velocityPer(FootPerHour)
  def mps(per: Per): A = velocityPer(MilePerSecond)
  def mpm(per: Per): A = velocityPer(MilePerMinute)
  def mph(per: Per): A = velocityPer(MilePerHour)
}

trait PredefinedVelocityUnit extends VelocityPostfixOps[VelocityUnit]{
  override protected def velocityPostfixOps(unit: VelocityUnit) = unit
  
}

object PredefinedVelocityUnit extends PredefinedVelocityUnit
