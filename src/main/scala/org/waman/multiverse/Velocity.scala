package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

trait VelocityPostfixOps[A]{
  def `m/s` : A
  def `km/h`: A
}

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends Quantity[A, VelocityUnit]
    with VelocityPostfixOps[A]  // for style like "velocity.`m/s`" and "velocity `m/s`"
    with LengthPostfixOps[DivisibleBy[TimeUnit, A]]  // for style like "velocity.m/s" ( = "velocity.m./(s)")
    with LengthPer[TimePostfixOps[A]]  // for style like "velocity m/s" ( = "velocity.m(/).s")
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  // for style like "velocity (m/s)" ( = "velocity.apply(m/s)")
  def apply(evalUnit: VelocityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.inMetrePerSecond) / real(evalUnit.inMetrePerSecond)

  def `m/s`  = apply(VelocityUnit.MetrePerSecond)
  def `km/h` = apply(VelocityUnit.KiloMetrePerHour)

  // for style like "velocity.m/s"
  private def callLength(lengthUnit: LengthUnit) = new DivisibleBy[TimeUnit, A]{
    override def /(timeUnit: TimeUnit): A = apply(lengthUnit / timeUnit)
  }

  override def ym = callLength(LengthUnit.YoctoMetre)
  override def zm = callLength(LengthUnit.ZeptoMetre)
  override def am = callLength(LengthUnit.AttoMetre)
  override def fm = callLength(LengthUnit.FemtoMetre)
  override def pm = callLength(LengthUnit.PicoMetre)
  override def nm = callLength(LengthUnit.NanoMetre)
  override def μm = callLength(LengthUnit.MicroMetre)
  override def mm = callLength(LengthUnit.MilliMetre)
  override def cm = callLength(LengthUnit.CentiMetre)
  override def dm = callLength(LengthUnit.DeciMetre)
  override def m  = callLength(LengthUnit.Metre)
  override def dam = callLength(LengthUnit.DecaMetre)
  override def hm = callLength(LengthUnit.HectoMetre)
  override def km = callLength(LengthUnit.KiloMetre)
  override def Mm = callLength(LengthUnit.MegaMetre)
  override def Gm = callLength(LengthUnit.GigaMetre)
  override def Tm = callLength(LengthUnit.TeraMetre)
  override def Pm = callLength(LengthUnit.PetaMetre)
  override def Em = callLength(LengthUnit.ExaMetre)
  override def Zm = callLength(LengthUnit.ZettaMetre)
  override def Ym = callLength(LengthUnit.YottaMetre)

  // microscopic
  override def Å = callLength(LengthUnit.Angstrom)
  override def μ = callLength(LengthUnit.Micron)

  // astronomy
  override def au = callLength(LengthUnit.AstronomicalUnit)
  override def ly = callLength(LengthUnit.LightYear)
  override def pc = callLength(LengthUnit.Parsec)

  // yard-pond
  override def pt = callLength(LengthUnit.Point)
  override def in = callLength(LengthUnit.Inch)
  override def ft = callLength(LengthUnit.Feet)
  override def yd = callLength(LengthUnit.Yard)
  override def mi = callLength(LengthUnit.Mile)
  override def NM = callLength(LengthUnit.NauticalMile)

  // for style like "velocity m/s"
  private def callLengthPer(lengthUnit: LengthUnit) = new TimePostfixOps[A]{
    override def ns     = apply(lengthUnit / TimeUnit.NanoSecond)
    override def μs     = apply(lengthUnit / TimeUnit.MicroSecond)
    override def ms     = apply(lengthUnit / TimeUnit.MilliSecond)
    override def s      = apply(lengthUnit / TimeUnit.Second)
    override def d      = apply(lengthUnit / TimeUnit.Day)
    override def minute = apply(lengthUnit / TimeUnit.Minute)
    override def h      = apply(lengthUnit / TimeUnit.Hour)
  }

  override def ym(per: Per) = callLengthPer(LengthUnit.YoctoMetre)
  override def zm(per: Per) = callLengthPer(LengthUnit.ZeptoMetre)
  override def am(per: Per) = callLengthPer(LengthUnit.AttoMetre)
  override def fm(per: Per) = callLengthPer(LengthUnit.FemtoMetre)
  override def pm(per: Per) = callLengthPer(LengthUnit.PicoMetre)
  override def nm(per: Per) = callLengthPer(LengthUnit.NanoMetre)
  override def µm(per: Per) = callLengthPer(LengthUnit.MicroMetre)
  override def mm(per: Per) = callLengthPer(LengthUnit.MilliMetre)
  override def cm(per: Per) = callLengthPer(LengthUnit.CentiMetre)
  override def dm(per: Per) = callLengthPer(LengthUnit.DeciMetre)
  override def m (per: Per) = callLengthPer(LengthUnit.Metre)
  override def dam(per: Per) = callLengthPer(LengthUnit.DecaMetre)
  override def hm(per: Per) = callLengthPer(LengthUnit.HectoMetre)
  override def km(per: Per) = callLengthPer(LengthUnit.KiloMetre)
  override def Mm(per: Per) = callLengthPer(LengthUnit.MegaMetre)
  override def Gm(per: Per) = callLengthPer(LengthUnit.MegaMetre)
  override def Tm(per: Per) = callLengthPer(LengthUnit.GigaMetre)
  override def Pm(per: Per) = callLengthPer(LengthUnit.PetaMetre)
  override def Em(per: Per) = callLengthPer(LengthUnit.ExaMetre)
  override def Zm(per: Per) = callLengthPer(LengthUnit.ZettaMetre)
  override def Ym(per: Per) = callLengthPer(LengthUnit.YottaMetre)

  // microscopic
  override def Å(per: Per) = callLengthPer(LengthUnit.Angstrom)
  override def μ(per: Per) = callLengthPer(LengthUnit.Micron)

  // astronomy
  override def au(per: Per) = callLengthPer(LengthUnit.AstronomicalUnit)
  override def ly(per: Per) = callLengthPer(LengthUnit.LightYear)
  override def pc(per: Per) = callLengthPer(LengthUnit.Parsec)

  // yard-pond
  override def pt(per: Per) = callLengthPer(LengthUnit.Point)
  override def in(per: Per) = callLengthPer(LengthUnit.Inch)
  override def ft(per: Per) = callLengthPer(LengthUnit.Feet)
  override def yd(per: Per) = callLengthPer(LengthUnit.Yard)
  override def mi(per: Per) = callLengthPer(LengthUnit.Mile)
  override def NM(per: Per) = callLengthPer(LengthUnit.NauticalMile)
}

sealed trait VelocityUnit extends PhysicalUnit{
  def inMetrePerSecond: Real
}

class QuotientVelocityUnit(val lengthUnit: LengthUnit, val timeUnit: TimeUnit)
    extends VelocityUnit with QuotientUnit[LengthUnit, TimeUnit]{

  override def numeratorUnit: LengthUnit = lengthUnit
  override def denominatorUnit: TimeUnit = timeUnit

  override def inMetrePerSecond: Real = lengthUnit.inMetre / timeUnit.inSecond
}

object VelocityUnit{

  case object MetrePerSecond extends QuotientVelocityUnit(LengthUnit.Metre, TimeUnit.Second)

  case object KiloMetrePerHour extends QuotientVelocityUnit(LengthUnit.KiloMetre, TimeUnit.Hour)

  def apply(lUnit: LengthUnit, tUnit: TimeUnit): VelocityUnit =
    new QuotientVelocityUnit(lUnit, tUnit)
}

trait PredefinedVelocityUnit{
  val `m/s`  = VelocityUnit.MetrePerSecond
  val `km/h` = VelocityUnit.KiloMetrePerHour
}

object PredefinedVelocityUnit extends PredefinedVelocityUnit

trait VelocityUnitInterpreter[A]
    extends VelocityPostfixOps[Velocity[A]]
    with UnitConverter[A]{

  def apply(unit: VelocityUnit): Velocity[A]

  def `m/s`  = apply(VelocityUnit.MetrePerSecond)
  def `km/h` = apply(VelocityUnit.KiloMetrePerHour)
}
