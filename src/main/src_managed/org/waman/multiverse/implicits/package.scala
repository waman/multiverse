package org.waman.multiverse

import scala.language.implicitConversions
import spire.math._

import org.waman.multiverse.unit.angle._
import org.waman.multiverse.unit.basic._
import org.waman.multiverse.unit.chemistry._
import org.waman.multiverse.unit.density._
import org.waman.multiverse.unit.electrics._
import org.waman.multiverse.unit.fluid._
import org.waman.multiverse.unit.magnetics._
import org.waman.multiverse.unit.mechanics._
import org.waman.multiverse.unit.optics._
import org.waman.multiverse.unit.radiation._
import org.waman.multiverse.unit.thermodynamics._

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
    def apply(unit: ActionUnit): Action[A] = new Action(value, unit)
    def apply(unit: EnergyUnit): Energy[A] = new Energy(value, unit)
    def apply(unit: ForceUnit): Force[A] = new Force(value, unit)
    def apply(unit: MomentumUnit): Momentum[A] = new Momentum(value, unit)
    def apply(unit: PowerUnit): Power[A] = new Power(value, unit)
    def apply(unit: TimeSquaredUnit): TimeSquared[A] = new TimeSquared(value, unit)
    def apply(unit: TorqueUnit): Torque[A] = new Torque(value, unit)
    def apply(unit: IlluminanceUnit): Illuminance[A] = new Illuminance(value, unit)
    def apply(unit: LuminanceUnit): Luminance[A] = new Luminance(value, unit)
    def apply(unit: LuminousFluxUnit): LuminousFlux[A] = new LuminousFlux(value, unit)
    def apply(unit: LuminousIntensityUnit): LuminousIntensity[A] = new LuminousIntensity(value, unit)
    def apply(unit: AbsorbedDoseUnit): AbsorbedDose[A] = new AbsorbedDose(value, unit)
    def apply(unit: EquivalentDoseUnit): EquivalentDose[A] = new EquivalentDose(value, unit)
    def apply(unit: EquivalentDoseRateUnit): EquivalentDoseRate[A] = new EquivalentDoseRate(value, unit)
    def apply(unit: ExposureUnit): Exposure[A] = new Exposure(value, unit)
    def apply(unit: RadioactivityUnit): Radioactivity[A] = new Radioactivity(value, unit)
    def apply(unit: AbsoluteTemperatureUnit): AbsoluteTemperature[A] = new AbsoluteTemperature(value, unit)
    def apply(unit: EntropyUnit): Entropy[A] = new Entropy(value, unit)
    def apply(unit: TemperatureUnit): Temperature[A] = new Temperature(value, unit)
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

  // Implicit conversions between unrelated units (like energy and absolute temperature)
  implicit def convertAngularVelocityToFrequency[A: Fractional](q: AngularVelocity[A]): Frequency[A] =
    q.toFrequency

  implicit def convertFrequencyToAngularVelocity[A: Fractional](q: Frequency[A]): AngularVelocity[A] =
    q.toAngularVelocity

  implicit def convertMassToEnergy[A: Fractional](q: Mass[A]): Energy[A] =
    q.toEnergy

  implicit def convertConductanceToResistance[A: Fractional](q: Conductance[A]): Resistance[A] =
    q.toResistance

  implicit def convertResistanceToConductance[A: Fractional](q: Resistance[A]): Conductance[A] =
    q.toConductance

  implicit def convertEnergyToAbsoluteTemperature[A: Fractional](q: Energy[A]): AbsoluteTemperature[A] =
    q.toAbsoluteTemperature

  implicit def convertEnergyToMass[A: Fractional](q: Energy[A]): Mass[A] =
    q.toMass

  implicit def convertAbsoluteTemperatureToEnergy[A: Fractional](q: AbsoluteTemperature[A]): Energy[A] =
    q.toEnergy

  implicit def convertAbsoluteTemperatureToTemperature[A: Fractional](q: AbsoluteTemperature[A]): Temperature[A] =
    q.toTemperature

}
