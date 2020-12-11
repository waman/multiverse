package multiverse.unitsystem

import scala.language.implicitConversions

import multiverse.unit.angle.Angle
import multiverse.unit.angle.AngularVelocity
import multiverse.unit.angle.Frequency
import multiverse.unit.angle.SolidAngle
import multiverse.unit.basic.Area
import multiverse.unit.basic.Length
import multiverse.unit.basic.Mass
import multiverse.unit.basic.Time
import multiverse.unit.basic.Velocity
import multiverse.unit.basic.Volume
import multiverse.unit.density.Density
import multiverse.unit.density.LineDensity
import multiverse.unit.fluid.DynamicViscosity
import multiverse.unit.fluid.KinematicViscosity
import multiverse.unit.fluid.Pressure
import multiverse.unit.fluid.VolumeFlow
import multiverse.unit.mechanics.Acceleration
import multiverse.unit.mechanics.AngularMomentum
import multiverse.unit.mechanics.Energy
import multiverse.unit.mechanics.Force
import multiverse.unit.mechanics.MassTorque
import multiverse.unit.mechanics.Momentum
import multiverse.unit.mechanics.Power
import multiverse.unit.mechanics.TimeSquared
import multiverse.unit.radiometry.Irradiance
import multiverse.unit.radiometry.SpectralIrradiance
import multiverse.unit.radioactivity.AbsorbedDose
import multiverse.unit.radioactivity.EquivalentDose
import multiverse.unit.radioactivity.EquivalentDoseRate
import multiverse.unit.radioactivity.Radioactivity

import multiverse.unit.angle.AngleUnitObjects.radian
import multiverse.unit.basic.TimeUnitObjects.second
import multiverse.unit.angle.FrequencyUnitObjects.heltz
import multiverse.unit.angle.SolidAngleUnitObjects.steradian
import multiverse.unit.basic.AreaUnitObjects.square_metre
import multiverse.unit.basic.LengthUnitObjects.metre
import multiverse.unit.basic.MassUnitObjects.kilogram
import multiverse.unit.basic.VolumeUnitObjects.cubic_metre
import multiverse.unit.fluid.PressureUnitObjects.pascal
import multiverse.unit.mechanics.TimeSquaredUnitObjects.second_squared
import multiverse.unit.mechanics.ForceUnitObjects.newton
import multiverse.unit.mechanics.EnergyUnitObjects.joule
import multiverse.unit.mechanics.PowerUnitObjects.watt
import multiverse.unit.radioactivity.AbsorbedDoseUnitObjects.gray
import multiverse.unit.radioactivity.EquivalentDoseUnitObjects.sievert
import multiverse.unit.radioactivity.RadioactivityUnitObjects.becquerel

/**
 * Evaluate quantities by metre, kilogram, and second.
 */
trait MKS extends UnitSystem{
  implicit def evaluateAngle[A: Fractional](q: Angle[A]): A = q(radian)
  implicit def evaluateAngularVelocity[A: Fractional](q: AngularVelocity[A]): A = q(radian / second)
  implicit def evaluateFrequency[A: Fractional](q: Frequency[A]): A = q(heltz)
  implicit def evaluateSolidAngle[A: Fractional](q: SolidAngle[A]): A = q(steradian)
  implicit def evaluateArea[A: Fractional](q: Area[A]): A = q(square_metre)
  implicit def evaluateLength[A: Fractional](q: Length[A]): A = q(metre)
  implicit def evaluateMass[A: Fractional](q: Mass[A]): A = q(kilogram)
  implicit def evaluateTime[A: Fractional](q: Time[A]): A = q(second)
  implicit def evaluateVelocity[A: Fractional](q: Velocity[A]): A = q(metre / second)
  implicit def evaluateVolume[A: Fractional](q: Volume[A]): A = q(cubic_metre)
  implicit def evaluateDensity[A: Fractional](q: Density[A]): A = q(kilogram / cubic_metre)
  implicit def evaluateLineDensity[A: Fractional](q: LineDensity[A]): A = q(kilogram / metre)
  implicit def evaluateDynamicViscosity[A: Fractional](q: DynamicViscosity[A]): A = q(pascal * second)
  implicit def evaluateKinematicViscosity[A: Fractional](q: KinematicViscosity[A]): A = q(square_metre / second)
  implicit def evaluatePressure[A: Fractional](q: Pressure[A]): A = q(pascal)
  implicit def evaluateVolumeFlow[A: Fractional](q: VolumeFlow[A]): A = q(cubic_metre / second)
  implicit def evaluateAcceleration[A: Fractional](q: Acceleration[A]): A = q(metre / second_squared)
  implicit def evaluateAngularMomentum[A: Fractional](q: AngularMomentum[A]): A = q(newton * metre * second)
  implicit def evaluateEnergy[A: Fractional](q: Energy[A]): A = q(joule)
  implicit def evaluateForce[A: Fractional](q: Force[A]): A = q(newton)
  implicit def evaluateMassTorque[A: Fractional](q: MassTorque[A]): A = q(kilogram * metre)
  implicit def evaluateMomentum[A: Fractional](q: Momentum[A]): A = q(newton * second)
  implicit def evaluatePower[A: Fractional](q: Power[A]): A = q(watt)
  implicit def evaluateTimeSquared[A: Fractional](q: TimeSquared[A]): A = q(second_squared)
  implicit def evaluateIrradiance[A: Fractional](q: Irradiance[A]): A = q(watt / square_metre)
  implicit def evaluateSpectralIrradiance[A: Fractional](q: SpectralIrradiance[A]): A = q(watt / square_metre / heltz)
  implicit def evaluateAbsorbedDose[A: Fractional](q: AbsorbedDose[A]): A = q(gray)
  implicit def evaluateEquivalentDose[A: Fractional](q: EquivalentDose[A]): A = q(sievert)
  implicit def evaluateEquivalentDoseRate[A: Fractional](q: EquivalentDoseRate[A]): A = q(sievert / second)
  implicit def evaluateRadioactivity[A: Fractional](q: Radioactivity[A]): A = q(becquerel)
}

object MKS extends MKS
