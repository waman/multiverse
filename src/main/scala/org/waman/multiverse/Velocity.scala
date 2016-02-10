package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._
import org.waman.multiverse.Context._

trait VelocityPostfixOps[A]{

  protected def velocityPostfixOps(velocityUnit: VelocityUnit): A

  def M = velocityPostfixOps(VelocityUnit.MachNumber)
  def c = velocityPostfixOps(VelocityUnit.SpeedOfLight)

  def kn = velocityPostfixOps(VelocityUnit.Knot)
  def kn(c: Context) = _kn(c)
  def _kn(c: Context): PartialFunction[Context, A] = {
    case Admiralty => velocityPostfixOps(VelocityUnit.Knot_Admiralty)
  }

  def ips = velocityPostfixOps(VelocityUnit.InchPerSecond)
  def ipm = velocityPostfixOps(VelocityUnit.InchPerMinute)
  def iph = velocityPostfixOps(VelocityUnit.InchPerHour)

  def fps = velocityPostfixOps(VelocityUnit.FootPerSecond)
  def fpm = velocityPostfixOps(VelocityUnit.FootPerMinute)
  def fph = velocityPostfixOps(VelocityUnit.FootPerHour)

  def mps = velocityPostfixOps(VelocityUnit.MilePerSecond)
  def mpm = velocityPostfixOps(VelocityUnit.MilePerMinute)
  def mph = velocityPostfixOps(VelocityUnit.MilePerHour)
}

trait VelocityPer[A] {

  protected def velocityPer(velocityUnit: VelocityUnit): A

  def c(per: Per) = velocityPer(VelocityUnit.SpeedOfLight)
}

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends Quantity[A, VelocityUnit]
    with VelocityPostfixOps[A]  // for style like "velocity.`m/s`" and "velocity `m/s`"
    with LengthPostfixOps[DivisibleByTime[A]]  // for style like "velocity.m/s" ( = "velocity.m./(s)")
    with LengthPer[TimePostfixOps[A]]  // for style like "velocity m/s" ( = "velocity.m(/).s")
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  // for style like "velocity (m/s)" ( = "velocity.apply(m/s)")
  def apply(evalUnit: VelocityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetrePerSecond) / real(evalUnit.unitInMetrePerSecond)

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) = apply(velocityUnit)

  // for style like "velocity.m/s"
  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = new DivisibleByTime[A]{
    override def /(timeUnit: TimeUnit): A = apply(lengthUnit / timeUnit)
  }

  // for style like "velocity m/s"
  override protected def lengthPer(lengthUnit: LengthUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit): A = apply(lengthUnit / timeUnit)
  }
}

sealed trait VelocityUnit extends PhysicalUnit
  with DivisibleByTime[AccelerationUnit]{

  def unitInMetrePerSecond: Real

  override protected lazy val baseUnit = LengthUnit.Metre / TimeUnit.Second
  override protected lazy val inBaseUnitAccessor = () => unitInMetrePerSecond

  override def /(timeUnit: TimeUnit) = AccelerationUnit(this, timeUnit)
}

object VelocityUnit{

  private[VelocityUnit] abstract class VelocityUnitImpl(val symbol: String, val unitInMetrePerSecond: Real)
    extends VelocityUnit{

    def this(symbol: String, lengthUnit: LengthUnit, timeUnit: TimeUnit) =
      this(symbol, lengthUnit.unitInMetre / timeUnit.unitInSecond)
  }

  case object MachNumber extends VelocityUnitImpl("M", r"340") with NotExact
  case object SpeedOfLight extends VelocityUnitImpl("c", r"299792458")

  case object Knot extends VelocityUnitImpl("kn", LengthUnit.NauticalMile, TimeUnit.Hour)
  case object Knot_Admiralty extends VelocityUnitImpl("kn(Adm)", LengthUnit.NauticalMile_Admiralty, TimeUnit.Hour)

  case object InchPerSecond extends VelocityUnitImpl("ips", LengthUnit.Inch, TimeUnit.Second)
  case object InchPerMinute extends VelocityUnitImpl("ipm", LengthUnit.Inch, TimeUnit.Minute)
  case object InchPerHour   extends VelocityUnitImpl("iph", LengthUnit.Inch, TimeUnit.Hour)

  case object FootPerSecond extends VelocityUnitImpl("fps", LengthUnit.Foot, TimeUnit.Second)
  case object FootPerMinute extends VelocityUnitImpl("fpm", LengthUnit.Foot, TimeUnit.Minute)
  case object FootPerHour   extends VelocityUnitImpl("fph", LengthUnit.Foot, TimeUnit.Hour)

  case object MilePerSecond extends VelocityUnitImpl("mps", LengthUnit.Mile, TimeUnit.Second)
  case object MilePerMinute extends VelocityUnitImpl("mpm", LengthUnit.Mile, TimeUnit.Minute)
  case object MilePerHour   extends VelocityUnitImpl("mph", LengthUnit.Mile, TimeUnit.Hour)

  // Length/Time
  private[VelocityUnit] class QuotientVelocityUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
    extends VelocityUnit with QuotientUnit[LengthUnit, TimeUnit]{

    override lazy val unitInMetrePerSecond: Real = numeratorUnit.unitInMetre / denominatorUnit.unitInSecond
  }

  def apply(lUnit: LengthUnit, tUnit: TimeUnit): VelocityUnit =
    new QuotientVelocityUnit(lUnit, tUnit)
}

trait PredefinedVelocityUnit extends VelocityPostfixOps[VelocityUnit]{
  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) = velocityUnit
}

object PredefinedVelocityUnit extends PredefinedVelocityUnit

trait VelocityUnitInterpreter[A]
    extends VelocityPostfixOps[Velocity[A]]
    with UnitConverter[A]{

  def apply(velocityUnit: VelocityUnit): Velocity[A]

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) =
    apply(velocityUnit)
}
