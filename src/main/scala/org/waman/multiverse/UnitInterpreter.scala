package org.waman.multiverse

import spire.math.{Fractional, Real}
import spire.implicits._

class UnitInterpreter[A: Fractional](val value: A)
  extends LengthUnitInterpreter[A]
  with TimeUnitInterpreter[A]
  with VelocityUnitInterpreter[A]
  with AngleUnitInterpreter[A]
  with AngularVelocityUnitInterpreter[A]{

  protected val algebra = implicitly[Fractional[A]]

  //***** Length *****
  protected def newMetreLength(a: A): Length[A] = MetreLength(a)
  private def newMetreLength(a: A, u: Real): Length[A] = newMetreLength(times(a, u))

  case class MetreLength(value: A) extends Length[A]{
    override def m: A = value
  }

  override def nm: Length[A] = newMetreLength(value, LengthUnit.Nanometre.inMetre)
  override def µm: Length[A] = newMetreLength(value, LengthUnit.Micrometre.inMetre)
  override def mm: Length[A] = newMetreLength(value, LengthUnit.Millimetre.inMetre)
  override def cm: Length[A] = newMetreLength(value, LengthUnit.Centimetre.inMetre)
  override def m : Length[A] = newMetreLength(value)
  override def km: Length[A] = newMetreLength(value, LengthUnit.Kilometre.inMetre)
  override def Mm: Length[A] = newMetreLength(value, LengthUnit.Megametre.inMetre)
  override def Gm: Length[A] = newMetreLength(value, LengthUnit.Gigametre.inMetre)
  override def Tm: Length[A] = newMetreLength(value, LengthUnit.Terametre.inMetre)

  // astronomy
  override def au: Length[A] = newMetreLength(value, LengthUnit.AstronomicalUnit.inMetre)
  override def ly: Length[A] = newMetreLength(value, LengthUnit.LightYear.inMetre)
  override def pc: Length[A] = newMetreLength(value, LengthUnit.Parsec.inMetre)

  // yard-pond
  override def in: Length[A] = newMetreLength(value, LengthUnit.Inch.inMetre)
  override def ft: Length[A] = newMetreLength(value, LengthUnit.Feet.inMetre)
  override def yd: Length[A] = newMetreLength(value, LengthUnit.Yard.inMetre)
  override def mi: Length[A] = newMetreLength(value, LengthUnit.Mile.inMetre)

  // Length -> Velocity
  override protected def newMetrePer(value: A, u: Real): TimePostfixOps[Velocity[A]] =
    new MetrePer(times(value, u))

  //***** Time *****
  protected def newSecondTime(a: A): Time[A] = SecondTime(a)
  private def newSecondTime(a: A, u: Real): Time[A] = newSecondTime(times(a, u))

  case class SecondTime(value: A) extends Time[A] {
    override def s: A = value
  }

  override def ns    : Time[A] = newSecondTime(value, TimeUnit.NanoSecond.inSecond)
  override def µs    : Time[A] = newSecondTime(value, TimeUnit.MicroSecond.inSecond)
  override def ms    : Time[A] = newSecondTime(value, TimeUnit.MilliSecond.inSecond)
  override def s     : Time[A] = newSecondTime(value)
  override def minute: Time[A] = newSecondTime(value, TimeUnit.Minute.inSecond)
  override def h     : Time[A] = newSecondTime(value, TimeUnit.Hour.inSecond)
  override def d     : Time[A] = newSecondTime(value, TimeUnit.Day.inSecond)

  //***** Velocity *****
  private def newMetrePerSecondVelocity(a: A): Velocity[A] = MetrePerSecondVelocity(a)
  private def newMetrePerSecondVelocity(a: A, u: Real): Velocity[A] = newMetrePerSecondVelocity(times(a, u))

  override def `m/s` : Velocity[A] = newMetrePerSecondVelocity(value)
  override def `km/h`: Velocity[A] = newMetrePerSecondVelocity(value, VelocityUnit.KilometrePerHour.inMetrePerSecond)
  //  def c     : Velocity = SpeedOfLight(value)

  override protected def newVelocity(value: A, unit: VelocityUnit): Velocity[A] =
    new UnitInterpreter[A](times(value, unit.inMetrePerSecond)).`m/s`

  //***** Angle *****
  override def deg: Angle[A] = RadianAngle(Angle.degreeToRadian(value))
  override def rad: Angle[A] = RadianAngle(value)

  case class RadianAngle(value: A) extends Angle[A]{
    override def rad: A = value
  }

  override def rad(per: Per): TimePostfixOps[AngularVelocity[A]] = ???

  //***** Angular Velocity *****

  override def `rad/s`: AngularVelocity[A] = ???
}