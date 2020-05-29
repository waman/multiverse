package waman.multiverse

import scala.language.implicitConversions
import spire.math._

import waman.multiverse.unit.angle._
import waman.multiverse.unit.basic._
import waman.multiverse.unit.chemistry._
import waman.multiverse.unit.density._
import waman.multiverse.unit.electrics._
import waman.multiverse.unit.fluid._
import waman.multiverse.unit.magnetics._
import waman.multiverse.unit.mechanics._
import waman.multiverse.unit.photometry._
import waman.multiverse.unit.radioactivity._
import waman.multiverse.unit.radiometry._
import waman.multiverse.unit.thermodynamics._

package object implicits {

  implicit class QuantityFactory[A: Fractional](val value: A){

    def apply(unit: AngleUnit): Angle[A] = new Angle(value, unit)
    def apply(unit: AngularVelocityUnit): AngularVelocity[A] = new AngularVelocity(value, unit)
    def apply(unit: FrequencyUnit): Frequency[A] = new Frequency(value, unit)
    def apply(unit: SolidAngleUnit): SolidAngle[A] = new SolidAngle(value, unit)
    def apply(unit: AreaUnit): Area[A] = new Area(value, unit)
    def apply(unit: LengthUnit): Length[A] = new Length(value, unit)
    def apply(unit: MassUnit): Mass[A] = new Mass(value, unit)
    def apply(unit: TimeUnit): Time[A] = new Time(value, unit)
    def apply(unit: VelocityUnit): Velocity[A] = new Velocity(value, unit)
    def apply(unit: VolumeUnit): Volume[A] = new Volume(value, unit)
    def apply(unit: AmountOfSubstanceUnit): AmountOfSubstance[A] = new AmountOfSubstance(value, unit)
    def apply(unit: CatalysisUnit): Catalysis[A] = new Catalysis(value, unit)
    def apply(unit: DensityUnit): Density[A] = new Density(value, unit)
    def apply(unit: LineDensityUnit): LineDensity[A] = new LineDensity(value, unit)
    def apply(unit: CapacitanceUnit): Capacitance[A] = new Capacitance(value, unit)
    def apply(unit: ChargeUnit): Charge[A] = new Charge(value, unit)
    def apply(unit: ConductanceUnit): Conductance[A] = new Conductance(value, unit)
    def apply(unit: CurrentUnit): Current[A] = new Current(value, unit)
    def apply(unit: DipoleUnit): Dipole[A] = new Dipole(value, unit)
    def apply(unit: ResistanceUnit): Resistance[A] = new Resistance(value, unit)
    def apply(unit: VoltageUnit): Voltage[A] = new Voltage(value, unit)
    def apply(unit: DynamicViscosityUnit): DynamicViscosity[A] = new DynamicViscosity(value, unit)
    def apply(unit: KinematicViscosityUnit): KinematicViscosity[A] = new KinematicViscosity(value, unit)
    def apply(unit: PressureUnit): Pressure[A] = new Pressure(value, unit)
    def apply(unit: VolumeFlowUnit): VolumeFlow[A] = new VolumeFlow(value, unit)
    def apply(unit: FluxUnit): Flux[A] = new Flux(value, unit)
    def apply(unit: FluxDensityUnit): FluxDensity[A] = new FluxDensity(value, unit)
    def apply(unit: InductanceUnit): Inductance[A] = new Inductance(value, unit)
    def apply(unit: AccelerationUnit): Acceleration[A] = new Acceleration(value, unit)
    def apply(unit: AngularMomentumUnit): AngularMomentum[A] = new AngularMomentum(value, unit)
    def apply(unit: EnergyUnit): Energy[A] = new Energy(value, unit)
    def apply(unit: ForceUnit): Force[A] = new Force(value, unit)
    def apply(unit: MassTorqueUnit): MassTorque[A] = new MassTorque(value, unit)
    def apply(unit: MomentumUnit): Momentum[A] = new Momentum(value, unit)
    def apply(unit: PowerUnit): Power[A] = new Power(value, unit)
    def apply(unit: TimeSquaredUnit): TimeSquared[A] = new TimeSquared(value, unit)
    def apply(unit: IlluminanceUnit): Illuminance[A] = new Illuminance(value, unit)
    def apply(unit: LuminanceUnit): Luminance[A] = new Luminance(value, unit)
    def apply(unit: LuminousFluxUnit): LuminousFlux[A] = new LuminousFlux(value, unit)
    def apply(unit: LuminousIntensityUnit): LuminousIntensity[A] = new LuminousIntensity(value, unit)
    def apply(unit: AbsorbedDoseUnit): AbsorbedDose[A] = new AbsorbedDose(value, unit)
    def apply(unit: EquivalentDoseUnit): EquivalentDose[A] = new EquivalentDose(value, unit)
    def apply(unit: EquivalentDoseRateUnit): EquivalentDoseRate[A] = new EquivalentDoseRate(value, unit)
    def apply(unit: ExposureUnit): Exposure[A] = new Exposure(value, unit)
    def apply(unit: RadioactivityUnit): Radioactivity[A] = new Radioactivity(value, unit)
    def apply(unit: AreaFrequencyUnit): AreaFrequency[A] = new AreaFrequency(value, unit)
    def apply(unit: IrradianceUnit): Irradiance[A] = new Irradiance(value, unit)
    def apply(unit: SpectralIrradianceUnit): SpectralIrradiance[A] = new SpectralIrradiance(value, unit)
    def apply(unit: AbsoluteTemperatureUnit): AbsoluteTemperature[A] = new AbsoluteTemperature(value, unit)
    def apply(unit: EntropyUnit): Entropy[A] = new Entropy(value, unit)
    def apply(unit: TemperatureUnit): Temperature[A] = new Temperature(value, unit)

    def apply(unit: TypelessLinearUnit): TypelessLinearQuantity[A] = new TypelessLinearQuantity(value, unit)
  }

  // An integral value (like 1(m), not 1.0(m)) create a Quantity[Real] instance
  implicit def convertIntToQuantityFactory(value: Int): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertLongToQuantityFactory(value: Long): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertSafeLongToQuantityFactory(value: SafeLong): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertBigIntToQuantityFactory(value: BigInt): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  // Implicit conversions between Temperature and AbsoluteTemperature)
  implicit def convertAbsoluteTemperatureToTemperature[A: Fractional](q: AbsoluteTemperature[A]): Temperature[A] =
    q.toTemperature

  implicit def convertTemperatureToAbsoluteTemperature[A: Fractional](q: Temperature[A]): AbsoluteTemperature[A] =
    q.toAbsoluteTemperature

}
