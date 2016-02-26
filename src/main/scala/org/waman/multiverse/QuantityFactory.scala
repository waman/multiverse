package org.waman.multiverse

import org.waman.multiverse.angle._
import org.waman.multiverse.electric._
import org.waman.multiverse.energy._
import org.waman.multiverse.fluid._
import org.waman.multiverse.luminous._
import org.waman.multiverse.magnetic._
import org.waman.multiverse.mass._
import org.waman.multiverse.mechanics._
import org.waman.multiverse.metric._
import org.waman.multiverse.radiation._
import org.waman.multiverse.thermal._
import org.waman.multiverse.time._
import spire.math.Fractional

class QuantityFactory[A: Fractional](protected val value: A)
  extends LengthFactory[A]
  with AreaFactory[A]
  with VolumeFactory[A]
  with AngleFactory[A]
  with SolidAngleFactory[A]
  with MassFactory[A]
  with DensityFactory[A]
  with TimeFactory[A]
  with TimeSquaredFactory[A]
  with FrequencyFactory[A]

  with VelocityFactory[A]
  with AngularVelocityFactory[A]
  with VolumeFlowFactory[A]
  with AccelerationFactory[A]
  with ForceFactory[A]
  with PressureFactory[A]
  with TorqueFactory[A]
  with EnergyFactory[A]
  with PowerFactory[A]
  with ActionFactory[A]

  with DynamicViscosityFactory[A]
  with KinematicViscosityFactory[A]
  with CurrentFactory[A]
  with ChargeFactory[A]
  with DipoleFactory[A]
  with VoltageFactory[A]
  with ResistanceFactory[A]
  with CapacitanceFactory[A]
  with FluxFactory[A]
  with FluxDensityFactory[A]

  with InductanceFactory[A]
  with TemperatureFactory[A]
  with EntropyFactory[A]
  with LuminousIntensityFactory[A]
  with LuminanceFactory[A]
  with LuminousFluxFactory[A]
  with IlluminanceFactory[A]
  with RadioactivityFactory[A]
  with ExposureFactory[A]
  with AbsorbedDoseFactory[A]

  with EquivalentDoseFactory[A]
  with EquivalentDoseRateFactory[A]
  with UnitConverter[A]{

  protected val algebra = implicitly[Fractional[A]]

  override def apply(unit: LengthUnit)             = new Length(value, unit)
  override def apply(unit: AreaUnit)               = new Area(value, unit)
  override def apply(unit: VolumeUnit)             = new Volume(value, unit)
  override def apply(unit: AngleUnit)              = new Angle(value, unit)
  override def apply(unit: SolidAngleUnit)         = new SolidAngle(value, unit)
  override def apply(unit: MassUnit)               = new Mass(value, unit)
  override def apply(unit: DensityUnit)            = new Density(value, unit)
  override def apply(unit: TimeUnit)               = new Time(value, unit)
  override def apply(unit: TimeSquaredUnit)        = new TimeSquared(value, unit)
  override def apply(unit: FrequencyUnit)          = new Frequency(value, unit)

  override def apply(unit: VelocityUnit)           = new Velocity(value, unit)
  override def apply(unit: AngularVelocityUnit)    = new AngularVelocity(value, unit)
  override def apply(unit: VolumeFlowUnit)         = new VolumeFlow(value, unit)
  override def apply(unit: AccelerationUnit)       = new Acceleration(value, unit)
  override def apply(unit: ForceUnit)              = new Force(value, unit)
  override def apply(unit: EnergyUnit)             = new Energy(value, unit)
  override def apply(unit: PressureUnit)           = new Pressure(value, unit)
  override def apply(unit: TorqueUnit)             = new Torque(value, unit)
  override def apply(unit: PowerUnit)              = new Power(value, unit)
  override def apply(unit: ActionUnit)             = new Action(value, unit)

  override def apply(unit: DynamicViscosityUnit)   = new DynamicViscosity(value, unit)
  override def apply(unit: KinematicViscosityUnit) = new KinematicViscosity(value, unit)
  override def apply(unit: CurrentUnit)            = new Current(value, unit)
  override def apply(unit: ChargeUnit)             = new Charge(value, unit)
  override def apply(unit: DipoleUnit)             = new Dipole(value, unit)
  override def apply(unit: VoltageUnit)            = new Voltage(value, unit)
  override def apply(unit: ResistanceUnit)         = new Resistance(value, unit)
  override def apply(unit: CapacitanceUnit)        = new Capacitance(value, unit)
  override def apply(unit: FluxUnit)               = new Flux(value, unit)
  override def apply(unit: FluxDensityUnit)        = new FluxDensity(value, unit)

  override def apply(unit: InductanceUnit)         = new Inductance(value, unit)
  override def apply(unit: TemperatureUnit)        = new Temperature(value, unit)
  override def apply(unit: EntropyUnit)            = new Entropy(value, unit)
  override def apply(unit: LuminousIntensityUnit)  = new LuminousIntensity(value, unit)
  override def apply(unit: LuminanceUnit)          = new Luminance(value, unit)
  override def apply(unit: LuminousFluxUnit)       = new LuminousFlux(value, unit)
  override def apply(unit: IlluminanceUnit)        = new Illuminance(value, unit)
  override def apply(unit: RadioactivityUnit)      = new Radioactivity(value, unit)
  override def apply(unit: ExposureUnit)           = new Exposure(value, unit)
  override def apply(unit: AbsorbedDoseUnit)       = new AbsorbedDose(value, unit)

  override def apply(unit: EquivalentDoseUnit)     = new EquivalentDose(value, unit)
  override def apply(unit: EquivalentDoseRateUnit) = new EquivalentDoseRate(value, unit)
}