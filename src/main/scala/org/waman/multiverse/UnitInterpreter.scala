package org.waman.multiverse

import spire.math.{Real, Fractional}
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
  private def newMetreLength(a: A, u: Real): Length[A] = newMetreLength(convertFrom(a, u))

  case class MetreLength(value: A) extends Length[A]{
    override def m: A = value
  }

  override def mm: Length[A] = newMetreLength(value, LengthUnit.mm.inMetre)
  override def cm: Length[A] = newMetreLength(value, LengthUnit.cm.inMetre)
  override def m : Length[A] = newMetreLength(value)
  override def km: Length[A] = newMetreLength(value, LengthUnit.km.inMetre)

  override def au: Length[A] = newMetreLength(value, LengthUnit.au.inMetre)
  override def ly: Length[A] = newMetreLength(value, LengthUnit.ly.inMetre)
  override def pc: Length[A] = newMetreLength(value, LengthUnit.pc.inMetre)

  override def in: Length[A] = newMetreLength(value, LengthUnit.in.inMetre)
  override def ft: Length[A] = newMetreLength(value, LengthUnit.ft.inMetre)
  override def yd: Length[A] = newMetreLength(value, LengthUnit.yd.inMetre)
  override def mi: Length[A] = newMetreLength(value, LengthUnit.mi.inMetre)

  // Length -> Velocity
  override def m(per: Per): LengthPer = new LengthPerImpl(value)

  class LengthPerImpl(metre: A) extends LengthPer{
    def s: Velocity[A] = new UnitInterpreter(metre).`m/s`
  }

  //***** Time *****
  protected def newSecondTime(a: A): Time[A] = SecondTime(a)
  private def newSecondTime(a: A, u: Double): Time[A] = newSecondTime(convertFrom(a, u))

  case class SecondTime(value: A) extends Time[A] {
    override def s: A = value
  }

  override def ms    : Time[A] = newSecondTime(value, 0.001)
  override def s     : Time[A] = newSecondTime(value)
  override def minute: Time[A] = newSecondTime(value, 60)
  override def h     : Time[A] = newSecondTime(value, 3600)

  //***** Velocity *****
  protected def newMetrePerSecondVelocity(a: A): Velocity[A] = MetrePerSecondVelocity(a)
  private def newMetrePerSecondVelocity(a: A, u: Double): Velocity[A] = newMetrePerSecondVelocity(convertFrom(a, u))

  case class MetrePerSecondVelocity(value: A) extends Velocity[A]{
    override def `m/s`: A = value
  }

  override def `m/s` : Velocity[A] = newMetrePerSecondVelocity(value)
  override def `km/h`: Velocity[A] = newMetrePerSecondVelocity(value, 1000.0 / 3600.0)
  //  def c     : Velocity = SpeedOfLight(value)

  //***** Angle *****
  override def deg: Angle[A] = RadianAngle(Angle.degreeToRadian(value))
  override def rad: Angle[A] = RadianAngle(value)

  case class RadianAngle(value: A) extends Angle[A]{
    override def rad: A = value
  }

  override def rad(per: Per): AnglePer = ???

  //***** Angular Velocity *****

  override def `rad/s`: AngularVelocity[A] = ???
}