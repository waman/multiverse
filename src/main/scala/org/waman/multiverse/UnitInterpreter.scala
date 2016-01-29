package org.waman.multiverse

import spire.math.Fractional

class UnitInterpreter[A: Fractional](protected val value: A)
    extends LengthUnitInterpreter[A]
    with AreaUnitInterpreter[A]
    with VolumeUnitInterpreter[A]
    with TimeUnitInterpreter[A]
    with VelocityUnitInterpreter[A]
    with AngleUnitInterpreter[A]
    with AngularVelocityUnitInterpreter[A]
    with SolidAngleUnitInterpreter[A]
    with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  //***** Length *****
  override def apply(lengthUnit: LengthUnit) = new Length(value, lengthUnit)

  //****** Area *****
  override def apply(areaUnit: AreaUnit): Area[A] = new Area(value, areaUnit)

  //***** Volume *****
  override def apply(volumeUnit: VolumeUnit): Volume[A] = new Volume(value, volumeUnit)

  //***** Time *****
  override def apply(timeUnit: TimeUnit) = new Time(value, timeUnit)

  //***** Velocity *****
  override def apply(velocityUnit: VelocityUnit) = new Velocity(value, velocityUnit)

  //***** Angle *****
  override def apply(angleUnit: AngleUnit) = new Angle(value, angleUnit)

  //***** Angular Velocity *****
  override def apply(unit: AngularVelocityUnit) = new AngularVelocity(value, unit)

  //***** Solid Angle *****
  override def apply(unit: SolidAngleUnit) = new SolidAngle(value, unit)
}