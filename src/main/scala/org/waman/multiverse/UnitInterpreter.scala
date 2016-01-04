package org.waman.multiverse

import spire.math.Fractional

class UnitInterpreter[A: Fractional](protected val value: A)
    extends LengthUnitInterpreter[A]
    with TimeUnitInterpreter[A]
    with VelocityUnitInterpreter[A]
    with AngleUnitInterpreter[A]
    with AngularVelocityUnitInterpreter[A]
    with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  //***** Length *****
  override def apply(lengthUnit: LengthUnit) = new Length(value, lengthUnit)

  // Length -> Velocity
  override protected def newLengthPer(lengthUnit: LengthUnit) = new TimePostfixOps[Velocity[A]]{
    override def ns     = apply(lengthUnit / TimeUnit.Nanosecond)
    override def µs     = apply(lengthUnit / TimeUnit.Microsecond)
    override def ms     = apply(lengthUnit / TimeUnit.Millisecond)
    override def s      = apply(lengthUnit / TimeUnit.Second)
    override def minute = apply(lengthUnit / TimeUnit.Minute)
    override def h      = apply(lengthUnit / TimeUnit.Hour)
    override def d      = apply(lengthUnit / TimeUnit.Day)
  }

  //***** Time *****
  override def apply(timeUnit: TimeUnit) = new Time(value, timeUnit)

  //***** Velocity *****
  override def apply(velocityUnit: VelocityUnit) = new Velocity(value, velocityUnit)

  //***** Angle *****
  override def apply(angleUnit: AngleUnit) = new Angle(value, angleUnit)

  override protected def newAnglePer(unit: AngleUnit): TimePostfixOps[AngularVelocity[A]] =
    new TimePostfixOps[AngularVelocity[A]] {
      override def ns     = apply(unit / TimeUnit.Nanosecond)
      override def µs     = apply(unit / TimeUnit.Microsecond)
      override def ms     = apply(unit / TimeUnit.Millisecond)
      override def s      = apply(unit / TimeUnit.Second)
      override def minute = apply(unit / TimeUnit.Minute)
      override def h      = apply(unit / TimeUnit.Hour)
      override def d      = apply(unit / TimeUnit.Day)
    }

  //***** Angular Velocity *****
  override def apply(unit: AngularVelocityUnit) = new AngularVelocity(value, unit)

}