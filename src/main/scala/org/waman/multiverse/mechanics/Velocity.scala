package org.waman.multiverse.mechanics

import org.waman.multiverse.Context._
import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPer, LengthPostfixOps, LengthUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait VelocityPostfixOps[A]{
  import VelocityPostfixOps._
  import VelocityUnit._

  protected def velocityPostfixOps(velocityUnit: VelocityUnit): A

  def M = velocityPostfixOps(MachNumber)
  def c = velocityPostfixOps(SpeedOfLight)

  def kn = velocityPostfixOps(Knot)
  def kn(c: Context) = velocityPostfixOps(_kn(c))

  def ips = velocityPostfixOps(InchPerSecond)
  def ipm = velocityPostfixOps(InchPerMinute)
  def iph = velocityPostfixOps(InchPerHour)

  def fps = velocityPostfixOps(FootPerSecond)
  def fpm = velocityPostfixOps(FootPerMinute)
  def fph = velocityPostfixOps(FootPerHour)

  def mps = velocityPostfixOps(MilePerSecond)
  def mpm = velocityPostfixOps(MilePerMinute)
  def mph = velocityPostfixOps(MilePerHour)
}

object VelocityPostfixOps{
  import VelocityUnit._

  lazy val _kn: PartialFunction[Context, VelocityUnit] = {
    case Admiralty => Knot_Admiralty
  }
}

trait VelocityPer[A] {
  import VelocityUnit._

  protected def velocityPer(velocityUnit: VelocityUnit): A

  def M(per: Per) = velocityPer(MachNumber)
  def c(per: Per) = velocityPer(SpeedOfLight)

  def kn(per: Per) = velocityPer(Knot)

  def ips(per: Per) = velocityPer(InchPerSecond)
  def ipm(per: Per) = velocityPer(InchPerMinute)
  def iph(per: Per) = velocityPer(InchPerHour)

  def fps(per: Per) = velocityPer(FootPerSecond)
  def fpm(per: Per) = velocityPer(FootPerMinute)
  def fph(per: Per) = velocityPer(FootPerHour)

  def mps(per: Per) = velocityPer(MilePerSecond)
  def mpm(per: Per) = velocityPer(MilePerMinute)
  def mph(per: Per) = velocityPer(MilePerHour)
}

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends Quantity[A, VelocityUnit]
    with VelocityPostfixOps[A]  // for style like "velocity.`m/s`" and "velocity `m/s`"
    with LengthPostfixOps[DivisibleByTimeUnit[A]]  // for style like "velocity.m/s" ( = "velocity.m./(s)")
    with LengthPer[TimePostfixOps[A]]  // for style like "velocity m/s" ( = "velocity.m(/).s")
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  // for style like "velocity (m/s)" ( = "velocity.apply(m/s)")
  def apply(evalUnit: VelocityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetrePerSecond) / real(evalUnit.unitInMetrePerSecond)

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) = apply(velocityUnit)

  // for style like "velocity.m/s"
  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = new DivisibleByTimeUnit[A]{
    override def /(timeUnit: TimeUnit): A = apply(lengthUnit / timeUnit)
  }

  // for style like "velocity m/s"
  override protected def lengthPer(lengthUnit: LengthUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit): A = apply(lengthUnit / timeUnit)
  }
}

sealed trait VelocityUnit extends PhysicalUnit[VelocityUnit]
  with DivisibleByTimeUnit[AccelerationUnit]{

  def unitInMetrePerSecond: Real

  override def baseUnit = LengthUnit.Metre / TimeUnit.Second
  override def valueInBaseUnit = unitInMetrePerSecond

  override def /(timeUnit: TimeUnit) = AccelerationUnit(this, timeUnit)
}

object VelocityUnit extends ConstantsDefined[VelocityUnit]{

  // Custom
  private[VelocityUnit]
  class IntrinsicVelocityUnit(val symbol: String, val unitInMetrePerSecond: Real)
    extends VelocityUnit{

    def this(symbol: String, lengthUnit: LengthUnit, timeUnit: TimeUnit) =
      this(symbol, lengthUnit.unitInMetre / timeUnit.unitInSecond)
  }

  case object MachNumber extends IntrinsicVelocityUnit("M", r"340") with NotExact
  case object SpeedOfLight extends IntrinsicVelocityUnit("c", r"299792458")

  case object Knot extends IntrinsicVelocityUnit("kn", LengthUnit.NauticalMile, TimeUnit.Hour)
  case object Knot_Admiralty extends IntrinsicVelocityUnit("kn(Adm)", LengthUnit.NauticalMile_Admiralty, TimeUnit.Hour)

  case object InchPerSecond extends IntrinsicVelocityUnit("ips", LengthUnit.Inch, TimeUnit.Second)
  case object InchPerMinute extends IntrinsicVelocityUnit("ipm", LengthUnit.Inch, TimeUnit.Minute)
  case object InchPerHour   extends IntrinsicVelocityUnit("iph", LengthUnit.Inch, TimeUnit.Hour)

  case object FootPerSecond extends IntrinsicVelocityUnit("fps", LengthUnit.Foot, TimeUnit.Second)
  case object FootPerMinute extends IntrinsicVelocityUnit("fpm", LengthUnit.Foot, TimeUnit.Minute)
  case object FootPerHour   extends IntrinsicVelocityUnit("fph", LengthUnit.Foot, TimeUnit.Hour)

  case object MilePerSecond extends IntrinsicVelocityUnit("mps", LengthUnit.Mile, TimeUnit.Second)
  case object MilePerMinute extends IntrinsicVelocityUnit("mpm", LengthUnit.Mile, TimeUnit.Minute)
  case object MilePerHour   extends IntrinsicVelocityUnit("mph", LengthUnit.Mile, TimeUnit.Hour)

  override lazy val values = Seq(
    MachNumber,
    SpeedOfLight,
    Knot,
    Knot_Admiralty,

    InchPerSecond,
    InchPerMinute,
    InchPerHour,

    FootPerSecond,
    FootPerMinute,
    FootPerHour,

    MilePerSecond,
    MilePerMinute,
    MilePerHour
  )

  // Length / Time -> Velocity
  private[VelocityUnit]
  class QuotientVelocityUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
    extends VelocityUnit with QuotientUnit[VelocityUnit, LengthUnit, TimeUnit]{

    override lazy val unitInMetrePerSecond: Real = numeratorUnit.unitInMetre / denominatorUnit.unitInSecond
  }

  def apply(lUnit: LengthUnit, tUnit: TimeUnit): VelocityUnit =
    new QuotientVelocityUnit(lUnit, tUnit)
}

trait PredefinedVelocityUnit extends VelocityPostfixOps[VelocityUnit]{
  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) = velocityUnit
}

object PredefinedVelocityUnit extends PredefinedVelocityUnit

trait VelocityFactory[A]
    extends VelocityPostfixOps[Velocity[A]]
    with UnitConverter[A]{

  def apply(velocityUnit: VelocityUnit): Velocity[A]

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) =
    apply(velocityUnit)
}
