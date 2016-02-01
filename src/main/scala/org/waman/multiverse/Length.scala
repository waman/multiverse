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

  protected def lengthPostfixOps(lengthUnit: LengthUnit): A

  def ym: A = lengthPostfixOps(LengthUnit.YoctoMetre)
  def zm: A = lengthPostfixOps(LengthUnit.ZeptoMetre)
  def am: A = lengthPostfixOps(LengthUnit.AttoMetre)
  def fm: A = lengthPostfixOps(LengthUnit.FemtoMetre)
  def pm: A = lengthPostfixOps(LengthUnit.PicoMetre)
  def nm: A = lengthPostfixOps(LengthUnit.NanoMetre)
  def μm: A = lengthPostfixOps(LengthUnit.MicroMetre)
  def mm: A = lengthPostfixOps(LengthUnit.MilliMetre)
  def cm: A = lengthPostfixOps(LengthUnit.CentiMetre)
  def dm: A = lengthPostfixOps(LengthUnit.DeciMetre)
  def m : A = lengthPostfixOps(LengthUnit.Metre)
  def dam: A = lengthPostfixOps(LengthUnit.DecaMetre)
  def hm: A = lengthPostfixOps(LengthUnit.HectoMetre)
  def km: A = lengthPostfixOps(LengthUnit.KiloMetre)
  def Mm: A = lengthPostfixOps(LengthUnit.MegaMetre)
  def Gm: A = lengthPostfixOps(LengthUnit.GigaMetre)
  def Tm: A = lengthPostfixOps(LengthUnit.TeraMetre)
  def Pm: A = lengthPostfixOps(LengthUnit.PetaMetre)
  def Em: A = lengthPostfixOps(LengthUnit.ExaMetre)
  def Zm: A = lengthPostfixOps(LengthUnit.ZettaMetre)
  def Ym: A = lengthPostfixOps(LengthUnit.YottaMetre)

  // microscopic
  def Å: A = lengthPostfixOps(LengthUnit.Angstrom)
  def μ: A = lengthPostfixOps(LengthUnit.Micron)

  // astronomy
  def au: A = lengthPostfixOps(LengthUnit.AstronomicalUnit)
  def ly: A = lengthPostfixOps(LengthUnit.LightYear)
  def pc: A = lengthPostfixOps(LengthUnit.Parsec)

  // yard-pond
  def pt: A = lengthPostfixOps(LengthUnit.Point)
  def in: A = lengthPostfixOps(LengthUnit.Inch)
  def ft: A = lengthPostfixOps(LengthUnit.Feet)
  def yd: A = lengthPostfixOps(LengthUnit.Yard)
  def mi: A = lengthPostfixOps(LengthUnit.Mile)
  def NM: A = lengthPostfixOps(LengthUnit.NauticalMile)
}

trait LengthPer[A]{

  protected def lengthPer(lengthUnit: LengthUnit): A

  def ym(per: Per):A = lengthPer(LengthUnit.YoctoMetre)
  def zm(per: Per):A = lengthPer(LengthUnit.ZeptoMetre)
  def am(per: Per):A = lengthPer(LengthUnit.AttoMetre)
  def fm(per: Per):A = lengthPer(LengthUnit.FemtoMetre)
  def pm(per: Per):A = lengthPer(LengthUnit.PicoMetre)
  def nm(per: Per):A = lengthPer(LengthUnit.NanoMetre)
  def μm(per: Per):A = lengthPer(LengthUnit.MicroMetre)
  def mm(per: Per):A = lengthPer(LengthUnit.MilliMetre)
  def cm(per: Per):A = lengthPer(LengthUnit.CentiMetre)
  def dm(per: Per):A = lengthPer(LengthUnit.DeciMetre)
  def m (per: Per):A = lengthPer(LengthUnit.Metre)
  def dam(per: Per):A = lengthPer(LengthUnit.DecaMetre)
  def hm(per: Per):A = lengthPer(LengthUnit.HectoMetre)
  def km(per: Per):A = lengthPer(LengthUnit.KiloMetre)
  def Mm(per: Per):A = lengthPer(LengthUnit.MegaMetre)
  def Gm(per: Per):A = lengthPer(LengthUnit.GigaMetre)
  def Tm(per: Per):A = lengthPer(LengthUnit.TeraMetre)
  def Pm(per: Per):A = lengthPer(LengthUnit.PetaMetre)
  def Em(per: Per):A = lengthPer(LengthUnit.ExaMetre)
  def Zm(per: Per):A = lengthPer(LengthUnit.ZettaMetre)
  def Ym(per: Per):A = lengthPer(LengthUnit.YottaMetre)

  // microscopic
  def Å(per: Per):A = lengthPer(LengthUnit.Angstrom)
  def μ(per: Per):A = lengthPer(LengthUnit.Micron)

  // astronomy
  def au(per: Per):A = lengthPer(LengthUnit.AstronomicalUnit)
  def ly(per: Per):A = lengthPer(LengthUnit.LightYear)
  def pc(per: Per):A = lengthPer(LengthUnit.Parsec)

  // yard-pond
  def pt(per: Per):A = lengthPer(LengthUnit.Point)
  def in(per: Per):A = lengthPer(LengthUnit.Inch)
  def ft(per: Per):A = lengthPer(LengthUnit.Feet)
  def yd(per: Per):A = lengthPer(LengthUnit.Yard)
  def mi(per: Per):A = lengthPer(LengthUnit.Mile)
  def NM(per: Per):A = lengthPer(LengthUnit.NauticalMile)
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
  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(lengthUnit)

  /** For style like <code>length (m)</code> where <code>length</code> is a Length object*/
  def apply(evalUnit: LengthUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetre) / real(evalUnit.unitInMetre)

  /** For style like <code>1.0.m/s</code> */
  override def /(timeUnit: TimeUnit): Velocity[A] = new Velocity(value, unit / timeUnit)
}

object Length{
  def prankLength: Length[Real] = new Length(r"1.61624e-35", LengthUnit.Metre)  // TODO
}

sealed abstract class LengthUnit(val symbol: String, val unitInMetre: Real)
    extends PhysicalUnit
    with DivisibleBy[TimeUnit, VelocityUnit]{ // for style like "1.0 (m/s)" ( = "1.0.apply(m./(s))")

  override protected val baseUnit = LengthUnit.Metre
  override protected val inBaseUnitAccessor = () => unitInMetre

  override def /(timeUnit: TimeUnit): VelocityUnit = VelocityUnit(this, timeUnit)
}

object LengthUnit{

  case object YoctoMetre extends LengthUnit("ym", r"1e-24")
  case object ZeptoMetre extends LengthUnit("zm", r"1e-21")
  case object AttoMetre  extends LengthUnit("am", r"1e-18")
  case object FemtoMetre extends LengthUnit("am", r"1e-15")
  case object PicoMetre  extends LengthUnit("pm", r"1e-12")
  case object NanoMetre  extends LengthUnit("nm", r"1e-9")
  case object MicroMetre extends LengthUnit("μm", r"1e-6")
  case object MilliMetre extends LengthUnit("mm", r"1e-3")
  case object CentiMetre extends LengthUnit("cm", r"1e-2")
  case object DeciMetre  extends LengthUnit("dm", r"1e-1")
  case object Metre      extends LengthUnit("m" , r"1")
  case object DecaMetre  extends LengthUnit("dam", r"10")
  case object HectoMetre extends LengthUnit("hm", r"1e2")
  case object KiloMetre  extends LengthUnit("km", r"1e3")
  case object MegaMetre  extends LengthUnit("Mm", r"1e6")
  case object GigaMetre  extends LengthUnit("Gm", r"1e9")
  case object TeraMetre  extends LengthUnit("Tm", r"1e12")
  case object PetaMetre  extends LengthUnit("Pm", r"1e15")
  case object ExaMetre   extends LengthUnit("Em", r"1e18")
  case object ZettaMetre extends LengthUnit("Zm", r"1e21")
  case object YottaMetre extends LengthUnit("Ym", r"1e24")

  // microscopic
  case object Angstrom extends LengthUnit("Å" , r"1e-10")
  case object Micron   extends LengthUnit("μ" , r"1e-6")

  // astronomy
  case object AstronomicalUnit extends LengthUnit("au", r"149597870700")
  case object LightYear        extends LengthUnit("ly", r"9.4607304725808e15")
  case object Parsec           extends LengthUnit("pc", r"3.08567782e16")

  // yard-pond
  case object Point extends LengthUnit("pt", r"1/72" * Inch.unitInMetre)
  case object Inch extends LengthUnit("in", r"0.0254")
  case object Feet extends LengthUnit("ft", r"0.3048")
  case object Yard extends LengthUnit("yd", r"0.9144")
  case object Mile extends LengthUnit("mi", r"1609.344")
  case object NauticalMile extends LengthUnit("NM", r"1852")
}

trait PredefinedLengthUnit{

  val ym = LengthUnit.YoctoMetre
  val zm = LengthUnit.ZeptoMetre
  val am = LengthUnit.AttoMetre
  val fm = LengthUnit.FemtoMetre
  val pm = LengthUnit.PicoMetre
  val nm = LengthUnit.NanoMetre
  val μm = LengthUnit.MicroMetre
  val mm = LengthUnit.MilliMetre
  val cm = LengthUnit.CentiMetre
  val dm = LengthUnit.DeciMetre
  val m  = LengthUnit.Metre
  val dam = LengthUnit.DecaMetre
  val hm = LengthUnit.HectoMetre
  val km = LengthUnit.KiloMetre
  val Mm = LengthUnit.MegaMetre
  val Gm = LengthUnit.GigaMetre
  val Tm = LengthUnit.TeraMetre
  val Pm = LengthUnit.PetaMetre
  val Em = LengthUnit.ExaMetre
  val Zm = LengthUnit.ZettaMetre
  val Ym = LengthUnit.YottaMetre

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

  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(lengthUnit)

  // Length -> Velocity
  // for style "1.0 m/s" ( = "1.0.m(/).s")
  protected def newLengthPer(lengthUnit: LengthUnit) = new TimePostfixOps[Velocity[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(lengthUnit / timeUnit)
  }

  override protected def lengthPer(lengthUnit: LengthUnit) = newLengthPer(lengthUnit)

  def apply(velocityUnit: VelocityUnit): Velocity[A]
}