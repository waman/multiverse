package org.waman.multiverse

import spire.implicits._
import spire.math.{Real, Fractional}

/*
 * +++++ For Length +++++
 * 1.0.m --- LengthUnitInterpreter.m
 * 1.0 m --- LengthUnitInterpreter.m
 * 1.0 (m) ( = 1.0.apply(m)) --- LengthUnitInterpreter.apply(LengthUnit)
 *
 * length.m --- Length.m
 * length m --- Length.m
 * length (m) ( = length.apply(m)) --- Length.apply(LengthUnit)
 *
 * +++++ For Velocity +++++
 * 1.0.m/s --- Length#/(TimeUnit)
 * 1.0 m/s ( = 1.0.m(/).s) --- LengthUnitInterpreter.m(Per): TimePostfixOps, TimePostfixOps.s
 * 1.0 (m/s) ( = 1.0.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, LengthUnitInterpreter.apply(VelocityUnit)
 *
 * velocity.m/s ( = velocity.m./(s)) --- Velocity.m: DivisibleByTime, DivisibleByTime./(s)
 * velocity m/s ( = velocity.m(/).s) --- Velocity.m(Per): TimePostfixOps, TimePostfixOps.s
 * velocity (m/s) ( = velocity.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, Velocity.apply(VelocityUnit)
 */

trait LengthPostfixOps[A]{

  def fm: A
  def pm: A
  def nm: A
  def μm: A
  def mm: A
  def cm: A
  def m : A
  def km: A
  def Mm: A
  def Gm: A
  def Tm: A

  // microscopic
  def Å: A
  def μ: A

  // astronomy
  def au: A
  def ly: A
  def pc: A

  // yard-pond
  def pt: A
  def in: A
  def ft: A
  def yd: A
  def mi: A
  def NM: A
}

trait LengthPer[A]{

  def fm(per: Per): A
  def pm(per: Per): A
  def nm(per: Per): A
  def µm(per: Per): A
  def mm(per: Per): A
  def cm(per: Per): A
  def m (per: Per): A
  def km(per: Per): A
  def Mm(per: Per): A
  def Gm(per: Per): A
  def Tm(per: Per): A

  // astronomy
  def Å(per: Per): A
  def μ(per: Per): A

  // astronomy
  def au(per: Per): A
  def ly(per: Per): A
  def pc(per: Per): A

  // yard-pond
  def pt(per: Per): A
  def in(per: Per): A
  def ft(per: Per): A
  def yd(per: Per): A
  def mi(per: Per): A
  def NM(per: Per): A
}

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends Quantity[A, LengthUnit]
    with LengthPostfixOps[A]
    with DivisibleBy[TimeUnit, Velocity[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  /**
    *  For style like <code>length.m</code> and <code>length m</code>
    *  where <code>length</code> is a Length object
    */
  override def fm: A = apply(LengthUnit.FemtoMetre)
  override def pm: A = apply(LengthUnit.PicoMetre)
  override def nm: A = apply(LengthUnit.NanoMetre)
  override def μm: A = apply(LengthUnit.MicroMetre)
  override def mm: A = apply(LengthUnit.MilliMetre)
  override def cm: A = apply(LengthUnit.CentiMetre)
  override def m : A = apply(LengthUnit.Metre)
  override def km: A = apply(LengthUnit.KiloMetre)
  override def Mm: A = apply(LengthUnit.MegaMetre)
  override def Gm: A = apply(LengthUnit.GigaMetre)
  override def Tm: A = apply(LengthUnit.TeraMetre)

  // microscopic
  override def Å: A = apply(LengthUnit.Angstrom)
  override def μ: A = apply(LengthUnit.Micron)

  // astronomy
  override def au: A = apply(LengthUnit.AstronomicalUnit)
  override def ly: A = apply(LengthUnit.LightYear)
  override def pc: A = apply(LengthUnit.Parsec)

  // yard-pond
  override def pt: A = apply(LengthUnit.Point)
  override def in: A = apply(LengthUnit.Inch)
  override def ft: A = apply(LengthUnit.Feet)
  override def yd: A = apply(LengthUnit.Yard)
  override def mi: A = apply(LengthUnit.Mile)
  override def NM: A = apply(LengthUnit.NauticalMile)

  /** For style like <code>length (m)</code> where <code>length</code> is a Length object*/
  def apply(evalUnit: LengthUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.inMetre) / real(evalUnit.inMetre)

  /** For style like <code>1.0.m/s</code> */
  override def /(timeUnit: TimeUnit): Velocity[A] = new Velocity(value, unit / timeUnit)
}

object Length{
  def prankLength: Length[Real] = new Length(r"1.61624e-35", LengthUnit.Metre)  // TODO
}

sealed abstract class LengthUnit(val name: String, val symbol: String, val inMetre: Real)
    extends PhysicalUnit
    with DivisibleBy[TimeUnit, VelocityUnit]{ // for style like "1.0 (m/s)" ( = "1.0.apply(m./(s))")

  override def /(timeUnit: TimeUnit): VelocityUnit = VelocityUnit(this, timeUnit)
}

object LengthUnit{

  case object FemtoMetre extends LengthUnit("FemtoMetre[Fermi]", "fm", r"1e-15")
  case object PicoMetre  extends LengthUnit("PicoMetre" , "pm", r"1e-12")
  case object NanoMetre  extends LengthUnit("NanoMetre" , "nm", r"1e-9")
  case object MicroMetre extends LengthUnit("MicroMetre", "μm", r"1e-6")
  case object MilliMetre extends LengthUnit("MilliMetre", "mm", r"1e-3")
  case object CentiMetre extends LengthUnit("CentiMetre", "cm", r"1e-2")
  case object Metre      extends LengthUnit("Metre"     , "m" , r"1")
  case object KiloMetre  extends LengthUnit("KiloMetre" , "km", r"1e3")
  case object MegaMetre  extends LengthUnit("MegaMetre" , "Mm", r"1e6")
  case object GigaMetre  extends LengthUnit("GigaMetre" , "Gm", r"1e9")
  case object TeraMetre  extends LengthUnit("TeraMetre" , "Tm", r"1e12")

  // microscopic
  case object Angstrom extends LengthUnit("Angstrom" , "Å" , r"1e-10")
  case object Micron   extends LengthUnit("Micron"   , "μ" , r"1e-6")

  // astronomy
  case object AstronomicalUnit extends LengthUnit("AstronomicalUnit", "au", r"149597870700")
  case object LightYear        extends LengthUnit("LightYear"       , "ly", r"9.4607304725808e15")
  case object Parsec           extends LengthUnit("Parsec"          , "pc", r"3.08567782e16")

  // yard-pond
  case object Point extends LengthUnit("Point", "pt", r"1/72" * Inch.inMetre)
  case object Inch extends LengthUnit("Inch", "in", r"0.0254")
  case object Feet extends LengthUnit("Feet", "ft", r"0.3048")
  case object Yard extends LengthUnit("Yard", "yd", r"0.9144")
  case object Mile extends LengthUnit("Mile", "mi", r"1609.344")
  case object NauticalMile extends LengthUnit("NauticalMile", "NM", r"1852")
}

trait PredefinedLengthUnit{

  val fm = LengthUnit.FemtoMetre
  val pm = LengthUnit.PicoMetre
  val nm = LengthUnit.NanoMetre
  val μm = LengthUnit.MicroMetre
  val mm = LengthUnit.MilliMetre
  val cm = LengthUnit.CentiMetre
  val m  = LengthUnit.Metre
  val km = LengthUnit.KiloMetre
  val Mm = LengthUnit.MegaMetre
  val Gm = LengthUnit.GigaMetre
  val Tm = LengthUnit.TeraMetre

  // microscopic
  val Å = LengthUnit.Angstrom
  val μ = LengthUnit.Micron

  // astronomy
  val au = LengthUnit.AstronomicalUnit
  val ly = LengthUnit.LightYear
  val pc = LengthUnit.Parsec

  // yard-pond
  val pt = LengthUnit.Point
  val in = LengthUnit.Inch
  val ft = LengthUnit.Feet
  val yd = LengthUnit.Yard
  val mi = LengthUnit.Mile
  val NM = LengthUnit.NauticalMile
}

object PredefinedLengthUnit extends PredefinedLengthUnit

trait LengthUnitInterpreter[A]
    extends LengthPostfixOps[Length[A]]
    with LengthPer[TimePostfixOps[Velocity[A]]]{  // for style like "1.0 m/s" ( = 1.0.m(/).s)

  // for style like "1.0 (m)" ( = "1.0.apply(m)")
  def apply(unit: LengthUnit): Length[A]

  override def fm = apply(LengthUnit.FemtoMetre)
  override def pm = apply(LengthUnit.PicoMetre)
  override def nm = apply(LengthUnit.NanoMetre)
  override def μm = apply(LengthUnit.MicroMetre)
  override def mm = apply(LengthUnit.MilliMetre)
  override def cm = apply(LengthUnit.CentiMetre)
  override def m  = apply(LengthUnit.Metre)
  override def km = apply(LengthUnit.KiloMetre)
  override def Mm = apply(LengthUnit.MegaMetre)
  override def Gm = apply(LengthUnit.GigaMetre)
  override def Tm = apply(LengthUnit.TeraMetre)

  // microscopic
  override def μ = apply(LengthUnit.Micron)
  override def Å = apply(LengthUnit.Angstrom)

  // astronomy
  override def au = apply(LengthUnit.AstronomicalUnit)
  override def ly = apply(LengthUnit.LightYear)
  override def pc = apply(LengthUnit.Parsec)

  // yard-pond
  override def pt = apply(LengthUnit.Point)
  override def in = apply(LengthUnit.Inch)
  override def ft = apply(LengthUnit.Feet)
  override def yd = apply(LengthUnit.Yard)
  override def mi = apply(LengthUnit.Mile)
  override def NM = apply(LengthUnit.NauticalMile)

  // Length -> Velocity
  // for style "1.0 m/s" ( = "1.0.m(/).s")
  protected def newLengthPer(lengthUnit: LengthUnit) = new TimePostfixOps[Velocity[A]]{
    override def ns     = apply(lengthUnit / TimeUnit.NanoSecond)
    override def μs     = apply(lengthUnit / TimeUnit.MicroSecond)
    override def ms     = apply(lengthUnit / TimeUnit.MilliSecond)
    override def s      = apply(lengthUnit / TimeUnit.Second)
    override def minute = apply(lengthUnit / TimeUnit.Minute)
    override def h      = apply(lengthUnit / TimeUnit.Hour)
    override def d      = apply(lengthUnit / TimeUnit.Day)
  }

  def apply(velocityUnit: VelocityUnit): Velocity[A]

  override def fm(per: Per) = newLengthPer(LengthUnit.FemtoMetre)
  override def pm(per: Per) = newLengthPer(LengthUnit.PicoMetre)
  override def nm(per: Per) = newLengthPer(LengthUnit.NanoMetre)
  override def µm(per: Per) = newLengthPer(LengthUnit.MicroMetre)
  override def mm(per: Per) = newLengthPer(LengthUnit.MilliMetre)
  override def cm(per: Per) = newLengthPer(LengthUnit.CentiMetre)
  override def m(per: Per)  = newLengthPer(LengthUnit.Metre)
  override def km(per: Per) = newLengthPer(LengthUnit.KiloMetre)
  override def Mm(per: Per) = newLengthPer(LengthUnit.MegaMetre)
  override def Gm(per: Per) = newLengthPer(LengthUnit.GigaMetre)
  override def Tm(per: Per) = newLengthPer(LengthUnit.TeraMetre)

  // microscopic
  override def Å(per: Per) = newLengthPer(LengthUnit.Angstrom)
  override def μ(per: Per) = newLengthPer(LengthUnit.Micron)

  // astronomy
  override def au(per: Per) = newLengthPer(LengthUnit.AstronomicalUnit)
  override def ly(per: Per) = newLengthPer(LengthUnit.LightYear)
  override def pc(per: Per) = newLengthPer(LengthUnit.Parsec)

  // yard-pond
  override def pt(per: Per) = newLengthPer(LengthUnit.Point)
  override def in(per: Per) = newLengthPer(LengthUnit.Inch)
  override def ft(per: Per) = newLengthPer(LengthUnit.Feet)
  override def yd(per: Per) = newLengthPer(LengthUnit.Yard)
  override def mi(per: Per) = newLengthPer(LengthUnit.Mile)
  override def NM(per: Per) = newLengthPer(LengthUnit.NauticalMile)
}