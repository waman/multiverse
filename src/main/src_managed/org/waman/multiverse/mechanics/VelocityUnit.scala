package org.waman.multiverse.mechanics

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.time._
import org.waman.multiverse.metric.LengthUnit._
import org.waman.multiverse.time.TimeUnit._

sealed trait VelocityUnit extends PhysicalUnit[VelocityUnit]
  with DivisibleByTimeUnit[AccelerationUnit]{

  override def getSIUnit = LengthUnit.Metre / TimeUnit.Second

  override def /(unit: TimeUnit) = AccelerationUnit(this, unit)
}

object VelocityUnit extends ConstantsDefined[VelocityUnit]{

  // intrinsic
  private[VelocityUnit]
  class IntrinsicVelocityUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends VelocityUnit{

    def this(name: String, symbols: Seq[String], unit: VelocityUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: VelocityUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object MachNumber extends IntrinsicVelocityUnit("MachNumber", Seq("M"), r"340") with NotExact
  case object SpeedOfLight extends IntrinsicVelocityUnit("SpeedOfLight", Seq("c"), r"299792458")
  case object Knot extends IntrinsicVelocityUnit("Knot", Seq("kn"), NauticalMile / Hour)
  case object Knot_Admiralty extends IntrinsicVelocityUnit("Knot_Admiralty", Seq("kn(Adm)"), NauticalMile_Admiralty / Hour)
  case object InchPerSecond extends IntrinsicVelocityUnit("InchPerSecond", Seq("ips"), Inch / Second)
  case object InchPerMinute extends IntrinsicVelocityUnit("InchPerMinute", Seq("ipm"), Inch / Minute)
  case object InchPerHour extends IntrinsicVelocityUnit("InchPerHour", Seq("iph"), Inch / Hour)
  case object FootPerSecond extends IntrinsicVelocityUnit("FootPerSecond", Seq("fps"), Foot / Second)
  case object FootPerMinute extends IntrinsicVelocityUnit("FootPerMinute", Seq("fpm"), Foot / Minute)
  case object FootPerHour extends IntrinsicVelocityUnit("FootPerHour", Seq("fph"), Foot / Hour)
  case object MilePerSecond extends IntrinsicVelocityUnit("MilePerSecond", Seq("mps"), Mile / Second)
  case object MilePerMinute extends IntrinsicVelocityUnit("MilePerMinute", Seq("mpm"), Mile / Minute)
  case object MilePerHour extends IntrinsicVelocityUnit("MilePerHour", Seq("mph"), Mile / Hour)

  override lazy val values = Seq(MachNumber, SpeedOfLight, Knot, Knot_Admiralty, InchPerSecond, InchPerMinute, InchPerHour, FootPerSecond, FootPerMinute, FootPerHour, MilePerSecond, MilePerMinute, MilePerHour)

  // LengthUnit / TimeUnit -> Velocity
  private[VelocityUnit]
  class QuotientLengthPerTimeUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
      extends VelocityUnit with QuotientUnit[VelocityUnit, LengthUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
  }

  def apply(nUnit: LengthUnit, dUnit: TimeUnit): VelocityUnit =
    new QuotientLengthPerTimeUnit(nUnit, dUnit)
}

trait MultiplicativeByVelocityUnit[R]{
  def *(unit: VelocityUnit): R
}

trait DivisibleByVelocityUnit[R]{
  def /(unit: VelocityUnit): R
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
  import org.waman.multiverse.metric.MetricContext
  import MetricContext._

  def kn(c: MetricContext): A = velocityPostfixOps(_kn(c))
}

object VelocityPostfixOps{
  import VelocityUnit._
  import org.waman.multiverse.metric.MetricContext
  import MetricContext._


  lazy val _kn : PartialFunction[MetricContext, VelocityUnit] = {
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
