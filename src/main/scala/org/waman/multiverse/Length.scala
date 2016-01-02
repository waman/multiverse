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
  def m(per: Per) : A
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

abstract class Length[A: Fractional]
  extends LengthPostfixOps[A]
  with DivisibleByTime[Velocity[A]]
  with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  /**
    *  For style like <code>length.m</code> and <code>length m</code>
    *  where <code>length</code> is a Length object
    */
  override def nm: A = div(m, LengthUnit.Nanometre.inMetre)
  override def µm: A = div(m, LengthUnit.Micrometre.inMetre)
  override def mm: A = div(m, LengthUnit.Millimetre.inMetre)
  override def cm: A = div(m, LengthUnit.Centimetre.inMetre)
  override def m : A
  override def km: A = div(m, LengthUnit.Kilometre.inMetre)
  override def Mm: A = div(m, LengthUnit.Megametre.inMetre)
  override def Gm: A = div(m, LengthUnit.Gigametre.inMetre)
  override def Tm: A = div(m, LengthUnit.Terametre.inMetre)

  // astronomy
  override def au: A = div(m, LengthUnit.AstronomicalUnit.inMetre)
  override def ly: A = div(m, LengthUnit.LightYear.inMetre)
  override def pc: A = div(m, LengthUnit.Parsec.inMetre)

  // yard-pond
  override def in: A = div(m, LengthUnit.Inch.inMetre)
  override def ft: A = div(m, LengthUnit.Feet.inMetre)
  override def yd: A = div(m, LengthUnit.Yard.inMetre)
  override def mi: A = div(m, LengthUnit.Mile.inMetre)

  /** For style like <code>length (m)</code> where <code>length</code> is a Length object*/
  def apply(unit: LengthUnit): A = unit.accept(this)

  /** For style like <code>1.0.m/s</code> */
  override def /(timeUnit: TimeUnit): Velocity[A] = MetrePerSecondVelocity(div(m, timeUnit.inSecond))
}

abstract class LengthUnit(val inMetre: Real = r"1")
    extends DivisibleByTime[VelocityUnit]{ // for style like "1.0 (m/s)" ( = "1.0.apply(m./(s))")

  def accept[A](l: Length[A]): A
  def accept[A](ui: LengthUnitInterpreter[A]): Length[A]

  override def /(timeUnit: TimeUnit): VelocityUnit = new VelocityUnit{
    override val inMetrePerSecond: Real = inMetre / timeUnit.inSecond
  }
}

object LengthUnit{

  case object Nanometre extends LengthUnit(r"1e-9") { // 1e-9 m/nm
    override def accept[A](l: Length[A]) = l.nm
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.nm
  }

  case object Micrometre extends LengthUnit(r"1e-6") {
    override def accept[A](l: Length[A]) = l.µm
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.µm
  }

  case object Millimetre extends LengthUnit(r"1e-3") {
    override def accept[A](l: Length[A]) = l.mm
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.mm
  }

  case object Centimetre extends LengthUnit(r"1e-2") {
    override def accept[A](l: Length[A]) = l.cm
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.cm
  }

  case object Metre  extends LengthUnit() {
    override def accept[A](l: Length[A]) = l.m
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.m
  }

  case object Kilometre extends LengthUnit(r"1e3") {
    override def accept[A](l: Length[A]) = l.km
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.km
  }

  case object Megametre extends LengthUnit(r"1e6") {
    override def accept[A](l: Length[A]) = l.Mm
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.Mm
  }

  case object Gigametre extends LengthUnit(r"1e9") {
    override def accept[A](l: Length[A]) = l.Gm
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.Gm
  }

  case object Terametre extends LengthUnit(r"1e12") {
    override def accept[A](l: Length[A]) = l.Tm
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.Tm
  }

  // astronomy
  case object AstronomicalUnit extends LengthUnit(r"149597870700") {
    override def accept[A](l: Length[A]) = l.au
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.au
  }

  case object LightYear extends LengthUnit(r"9.4607304725808e15") {
    override def accept[A](l: Length[A]) = l.ly
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.ly
  }

  case object Parsec extends LengthUnit(r"3.08567782e16") {
    override def accept[A](l: Length[A]) = l.pc
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.pc
  }

  // yard-pond
  case object Inch extends LengthUnit(r"0.0254") {
    override def accept[A](l: Length[A]) = l.in
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.in
  }

  case object Feet extends LengthUnit(r"0.3048") {
    override def accept[A](l: Length[A]) = l.ft
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.ft
  }

  case object Yard extends LengthUnit(r"0.9144") {
    override def accept[A](l: Length[A]) = l.yd
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.yd
  }

  case object Mile extends LengthUnit(r"1609.344") {
    override def accept[A](l: Length[A]) = l.mi
    override def accept[A](ui: LengthUnitInterpreter[A]) = ui.mi
  }
}

class MetrePer[A: Fractional](metre: A)
    extends TimePostfixOps[Velocity[A]]
    with UnitConverter[A]{

  override protected val algebra: Fractional[A] = implicitly[Fractional[A]]

  private def newMetrePerSecondVelocity(a: A): Velocity[A] = MetrePerSecondVelocity(a)
  private def newMetrePerSecondVelocity(a: A, u: Real): Velocity[A] = newMetrePerSecondVelocity(div(a, u))

  override def ns    : Velocity[A] = newMetrePerSecondVelocity(metre, TimeUnit.NanoSecond.inSecond)
  override def µs    : Velocity[A] = newMetrePerSecondVelocity(metre, TimeUnit.MicroSecond.inSecond)
  override def ms    : Velocity[A] = newMetrePerSecondVelocity(metre, TimeUnit.MilliSecond.inSecond)
  override def s     : Velocity[A] = newMetrePerSecondVelocity(metre)
  override def minute: Velocity[A] = newMetrePerSecondVelocity(metre, TimeUnit.Minute.inSecond)
  override def h     : Velocity[A] = newMetrePerSecondVelocity(metre, TimeUnit.Hour.inSecond)
  override def d     : Velocity[A] = newMetrePerSecondVelocity(metre, TimeUnit.Day.inSecond)
}

trait LengthUnitInterpreter[A]
  extends LengthPostfixOps[Length[A]]
  with LengthPer[TimePostfixOps[Velocity[A]]]  // for style like "1.0 m/s" ( = 1.0.m(/).s)
  with UnitConverter[A]{

  val value: A

  // for style "1.0 m/s" ( = "1.0.m(/).s")
  protected def newMetrePer(value: A, u: Real = r"1"): TimePostfixOps[Velocity[A]]


  override def nm(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Nanometre.inMetre)
  override def µm(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Micrometre.inMetre)
  override def mm(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Millimetre.inMetre)
  override def cm(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Centimetre.inMetre)
  override def m(per: Per) : TimePostfixOps[Velocity[A]] = newMetrePer(value)
  override def km(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Kilometre.inMetre)
  override def Mm(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Megametre.inMetre)
  override def Gm(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Gigametre.inMetre)
  override def Tm(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Terametre.inMetre)

  // astronomy
  override def au(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.AstronomicalUnit.inMetre)
  override def ly(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.LightYear.inMetre)
  override def pc(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Parsec.inMetre)

  // yard-pond
  override def in(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Inch.inMetre)
  override def ft(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Feet.inMetre)
  override def yd(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Yard.inMetre)
  override def mi(per: Per): TimePostfixOps[Velocity[A]] = newMetrePer(value, LengthUnit.Mile.inMetre)

  // for style like "1.0 (m)" ( = "1.0.apply(m)")
  def apply(unit: LengthUnit): Length[A] = unit.accept(this)
}