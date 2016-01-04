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

  def nm: A
  def µm: A
  def mm: A
  def cm: A
  def m : A
  def km: A
  def Mm: A
  def Gm: A
  def Tm: A

  // astronomy
  def au: A
  def ly: A
  def pc: A

  // yard-pond
  def in: A
  def ft: A
  def yd: A
  def mi: A
}

trait LengthPer[A]{

  def nm(per: Per): A
  def µm(per: Per): A
  def mm(per: Per): A
  def cm(per: Per): A
  def m (per: Per) : A
  def km(per: Per): A
  def Mm(per: Per): A
  def Gm(per: Per): A
  def Tm(per: Per): A

  // astronomy
  def au(per: Per): A
  def ly(per: Per): A
  def pc(per: Per): A

  // yard-pond
  def in(per: Per): A
  def ft(per: Per): A
  def yd(per: Per): A
  def mi(per: Per): A
}

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends ValueWithUnit[A, LengthUnit]
    with LengthPostfixOps[A]
    with DivisibleByTime[Velocity[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  /**
    *  For style like <code>length.m</code> and <code>length m</code>
    *  where <code>length</code> is a Length object
    */
  override def nm: A = apply(LengthUnit.Nanometre)
  override def µm: A = apply(LengthUnit.Micrometre)
  override def mm: A = apply(LengthUnit.Millimetre)
  override def cm: A = apply(LengthUnit.Centimetre)
  override def m : A = apply(LengthUnit.Metre)
  override def km: A = apply(LengthUnit.Kilometre)
  override def Mm: A = apply(LengthUnit.Megametre)
  override def Gm: A = apply(LengthUnit.Gigametre)
  override def Tm: A = apply(LengthUnit.Terametre)

  // astronomy
  override def au: A = apply(LengthUnit.AstronomicalUnit)
  override def ly: A = apply(LengthUnit.LightYear)
  override def pc: A = apply(LengthUnit.Parsec)

  // yard-pond
  override def in: A = apply(LengthUnit.Inch)
  override def ft: A = apply(LengthUnit.Feet)
  override def yd: A = apply(LengthUnit.Yard)
  override def mi: A = apply(LengthUnit.Mile)

  /** For style like <code>length (m)</code> where <code>length</code> is a Length object*/
  def apply(evalUnit: LengthUnit): A =
    if(evalUnit == unit) value
    else value * real(unit.inMetre) / real(evalUnit.inMetre)

  /** For style like <code>1.0.m/s</code> */
  override def /(timeUnit: TimeUnit): Velocity[A] = new Velocity(value, unit / timeUnit)
}

abstract class LengthUnit(val name: String, val symbol: String, val inMetre: Real)
    extends PhysicalUnit
    with DivisibleByTime[VelocityUnit]{ // for style like "1.0 (m/s)" ( = "1.0.apply(m./(s))")

  override def /(timeUnit: TimeUnit): VelocityUnit = VelocityUnit(this, timeUnit)
}

object LengthUnit{

  case object Nanometre  extends LengthUnit("Nanometre" , "nm", r"1e-9")
  case object Micrometre extends LengthUnit("Micrometre", "µm", r"1e-6")
  case object Millimetre extends LengthUnit("Millimetre", "mm", r"1e-3")
  case object Centimetre extends LengthUnit("Centimetre", "cm", r"1e-2")
  case object Metre      extends LengthUnit("Metre"     , "m" , r"1")
  case object Kilometre  extends LengthUnit("Kilometre" , "km", r"1e3")
  case object Megametre  extends LengthUnit("Megametre" , "Mm", r"1e6")
  case object Gigametre  extends LengthUnit("Gigametre" , "Gm", r"1e9")
  case object Terametre  extends LengthUnit("Terametre" , "Tm", r"1e12")

  // astronomy
  case object AstronomicalUnit extends LengthUnit("AstronomicalUnit", "au", r"149597870700")
  case object LightYear        extends LengthUnit("LightYear"       , "ly", r"9.4607304725808e15")
  case object Parsec           extends LengthUnit("Parsec"          , "pc", r"3.08567782e16")

  // yard-pond
  case object Inch extends LengthUnit("Inch", "in", r"0.0254")
  case object Feet extends LengthUnit("Feet", "ft", r"0.3048")
  case object Yard extends LengthUnit("Yard", "yd", r"0.9144")
  case object Mile extends LengthUnit("Mile", "mi", r"1609.344")
}

trait LengthUnitInterpreter[A]
    extends LengthPostfixOps[Length[A]]
    with LengthPer[TimePostfixOps[Velocity[A]]]{  // for style like "1.0 m/s" ( = 1.0.m(/).s)

  // for style like "1.0 (m)" ( = "1.0.apply(m)")
  def apply(unit: LengthUnit): Length[A]

  override def nm = apply(LengthUnit.Nanometre)
  override def µm = apply(LengthUnit.Micrometre)
  override def mm = apply(LengthUnit.Millimetre)
  override def cm = apply(LengthUnit.Centimetre)
  override def m  = apply(LengthUnit.Metre)
  override def km = apply(LengthUnit.Kilometre)
  override def Mm = apply(LengthUnit.Megametre)
  override def Gm = apply(LengthUnit.Gigametre)
  override def Tm = apply(LengthUnit.Terametre)

  // astronomy
  override def au = apply(LengthUnit.AstronomicalUnit)
  override def ly = apply(LengthUnit.LightYear)
  override def pc = apply(LengthUnit.Parsec)

  // yard-pond
  override def in = apply(LengthUnit.Inch)
  override def ft = apply(LengthUnit.Feet)
  override def yd = apply(LengthUnit.Yard)
  override def mi = apply(LengthUnit.Mile)

  // for style "1.0 m/s" ( = "1.0.m(/).s")
  protected def newLengthPer(unit: LengthUnit): TimePostfixOps[Velocity[A]]

  override def nm(per: Per) = newLengthPer(LengthUnit.Nanometre)
  override def µm(per: Per) = newLengthPer(LengthUnit.Micrometre)
  override def mm(per: Per) = newLengthPer(LengthUnit.Millimetre)
  override def cm(per: Per) = newLengthPer(LengthUnit.Centimetre)
  override def m(per: Per)  = newLengthPer(LengthUnit.Metre)
  override def km(per: Per) = newLengthPer(LengthUnit.Kilometre)
  override def Mm(per: Per) = newLengthPer(LengthUnit.Megametre)
  override def Gm(per: Per) = newLengthPer(LengthUnit.Gigametre)
  override def Tm(per: Per) = newLengthPer(LengthUnit.Terametre)

  // astronomy
  override def au(per: Per) = newLengthPer(LengthUnit.AstronomicalUnit)
  override def ly(per: Per) = newLengthPer(LengthUnit.LightYear)
  override def pc(per: Per) = newLengthPer(LengthUnit.Parsec)

  // yard-pond
  override def in(per: Per) = newLengthPer(LengthUnit.Inch)
  override def ft(per: Per) = newLengthPer(LengthUnit.Feet)
  override def yd(per: Per) = newLengthPer(LengthUnit.Yard)
  override def mi(per: Per) = newLengthPer(LengthUnit.Mile)
}