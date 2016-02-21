package org.waman.multiverse

import org.waman.multiverse.angle._
import org.waman.multiverse.electric._
import org.waman.multiverse.energy._
import org.waman.multiverse.fluid._
import org.waman.multiverse.mass._
import org.waman.multiverse.mechanics._
import org.waman.multiverse.metric._
import org.waman.multiverse.time._
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
  with TimeSquaredUnitInterpreter[A]
  with FrequencyUnitInterpreter[A]
  with VelocityUnitInterpreter[A]
  with AngularVelocityUnitInterpreter[A]
  with VolumeFlowUnitInterpreter[A]
  with AccelerationUnitInterpreter[A]
  with ForceUnitInterpreter[A]
  with PressureUnitInterpreter[A]
  with TorqueUnitInterpreter[A]
  with EnergyUnitInterpreter[A]
  with PowerUnitInterpreter[A]
  with ActionUnitInterpreter[A]
  with DynamicViscosityUnitInterpreter[A]
  with KinematicViscosityUnitInterpreter[A]
  with CurrentUnitInterpreter[A]
  with ChargeUnitInterpreter[A]
  with DipoleUnitInterpreter[A]
  with VoltageUnitInterpreter[A]
  with ResistanceUnitInterpreter[A]
  with CapacitanceUnitInterpreter[A]
  with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  override def apply(lengthUnit: LengthUnit)             = new Length(value, lengthUnit)
  override def apply(areaUnit: AreaUnit)                 = new Area(value, areaUnit)
  override def apply(volumeUnit: VolumeUnit)             = new Volume(value, volumeUnit)
  override def apply(angleUnit: AngleUnit)               = new Angle(value, angleUnit)
  override def apply(solidAngleUnit: SolidAngleUnit)     = new SolidAngle(value, solidAngleUnit)
  override def apply(massUnit: MassUnit)                 = new Mass(value, massUnit)
  override def apply(densityUnit: DensityUnit)           = new Density(value, densityUnit)
  override def apply(timeUnit: TimeUnit)                 = new Time(value, timeUnit)
  override def apply(timeSquaredUnit: TimeSquaredUnit)   = new TimeSquared(value, timeSquaredUnit)
  override def apply(frequencyUnit: FrequencyUnit)       = new Frequency(value, frequencyUnit)
  override def apply(velocityUnit: VelocityUnit)         = new Velocity(value, velocityUnit)
  override def apply(avUnit: AngularVelocityUnit)        = new AngularVelocity(value, avUnit)
  override def apply(volumeFlowUnit: VolumeFlowUnit)     = new VolumeFlow(value, volumeFlowUnit)
  override def apply(accelerationUnit: AccelerationUnit) = new Acceleration(value, accelerationUnit)
  override def apply(forceUnit: ForceUnit)               = new Force(value, forceUnit)
  override def apply(energyUnit: EnergyUnit)             = new Energy(value, energyUnit)
  override def apply(pressureUnit: PressureUnit)         = new Pressure(value, pressureUnit)
  override def apply(torqueUnit: TorqueUnit)             = new Torque(value, torqueUnit)
  override def apply(powerUnit: PowerUnit)               = new Power(value, powerUnit)
  override def apply(actionUnit: ActionUnit)             = new Action(value, actionUnit)
  override def apply(dvUnit: DynamicViscosityUnit)       = new DynamicViscosity(value, dvUnit)
  override def apply(kvUnit: KinematicViscosityUnit)     = new KinematicViscosity(value, kvUnit)
  override def apply(ecUnit: CurrentUnit)                = new Current(value, ecUnit)
  override def apply(ecUnit: ChargeUnit)                 = new Charge(value, ecUnit)
  override def apply(edUnit: DipoleUnit)                 = new Dipole(value, edUnit)
  override def apply(voltageUnit: VoltageUnit)           = new Voltage(value, voltageUnit)
  override def apply(resistanceUnit: ResistanceUnit)     = new Resistance(value, resistanceUnit)
  override def apply(capacitanceUnit: CapacitanceUnit)   = new Capacitance(value, capacitanceUnit)
}