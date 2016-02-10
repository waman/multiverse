package org.waman.multiverse

import spire.math.Fractional

class UnitInterpreter[A: Fractional](protected val value: A)
  extends LengthUnitInterpreter[A]
  with AreaUnitInterpreter[A]
  with VolumeUnitInterpreter[A]
  with AngleUnitInterpreter[A]
  with SolidAngleUnitInterpreter[A]
  with MassUnitInterpreter[A]
  with DensityUnitInterpreter[A]
  with TimeUnitInterpreter[A]
  with FrequencyUnitInterpreter[A]
  with VelocityUnitInterpreter[A]
  with AngularVelocityUnitInterpreter[A]
  with VolumeFlowUnitInterpreter[A]
  with AccelerationUnitInterpreter[A]
  with ForceUnitInterpreter[A]
  with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  override def apply(lengthUnit: LengthUnit)                   = new Length(value, lengthUnit)
  override def apply(areaUnit: AreaUnit)                       = new Area(value, areaUnit)
  override def apply(volumeUnit: VolumeUnit)                   = new Volume(value, volumeUnit)
  override def apply(angleUnit: AngleUnit)                     = new Angle(value, angleUnit)
  override def apply(solidAngleUnit: SolidAngleUnit)           = new SolidAngle(value, solidAngleUnit)
  override def apply(massUnit: MassUnit)                       = new Mass(value, massUnit)
  override def apply(densityUnit: DensityUnit)                 = new Density(value, densityUnit)
  override def apply(timeUnit: TimeUnit)                       = new Time(value, timeUnit)
  override def apply(frequencyUnit: FrequencyUnit)             = new Frequency(value, frequencyUnit)
  override def apply(velocityUnit: VelocityUnit)               = new Velocity(value, velocityUnit)
  override def apply(angularVelocityUnit: AngularVelocityUnit) = new AngularVelocity(value, angularVelocityUnit)
  override def apply(volumeFlowUnit: VolumeFlowUnit)           = new VolumeFlow(value, volumeFlowUnit)
  override def apply(accelerationUnit: AccelerationUnit)       = new Acceleration(value, accelerationUnit)
  override def apply(forceUnit: ForceUnit)                     = new Force(value, forceUnit)
}