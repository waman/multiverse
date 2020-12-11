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
import multiverse.unit.basic.AreaUnitObjects.square_centimetre
import multiverse.unit.basic.LengthUnitObjects.centimetre
import multiverse.unit.basic.MassUnitObjects.gram
import multiverse.unit.basic.VolumeUnitObjects.cubic_centimetre
import multiverse.unit.fluid.DynamicViscosityUnitObjects.poise
import multiverse.unit.fluid.KinematicViscosityUnitObjects.stokes
import multiverse.unit.fluid.PressureUnitObjects.barye
import multiverse.unit.mechanics.AccelerationUnitObjects.gal
import multiverse.unit.mechanics.ForceUnitObjects.dyne
import multiverse.unit.mechanics.EnergyUnitObjects.erg
import multiverse.unit.mechanics.TimeSquaredUnitObjects.second_squared
import multiverse.unit.radioactivity.AbsorbedDoseUnitObjects.rad
import multiverse.unit.radioactivity.EquivalentDoseUnitObjects.roentgen_equivalent_man
import multiverse.unit.radioactivity.RadioactivityUnitObjects.curie

trait CGS extends UnitSystem{
  implicit def evaluateAngle[A: Fractional](q: Angle[A]): A = q(radian)
  implicit def evaluateAngularVelocity[A: Fractional](q: AngularVelocity[A]): A = q(radian / second)
  implicit def evaluateFrequency[A: Fractional](q: Frequency[A]): A = q(heltz)
  implicit def evaluateSolidAngle[A: Fractional](q: SolidAngle[A]): A = q(steradian)
  implicit def evaluateArea[A: Fractional](q: Area[A]): A = q(square_centimetre)
  implicit def evaluateLength[A: Fractional](q: Length[A]): A = q(centimetre)
  implicit def evaluateMass[A: Fractional](q: Mass[A]): A = q(gram)
  implicit def evaluateTime[A: Fractional](q: Time[A]): A = q(second)
  implicit def evaluateVelocity[A: Fractional](q: Velocity[A]): A = q(centimetre / second)
  implicit def evaluateVolume[A: Fractional](q: Volume[A]): A = q(cubic_centimetre)
  implicit def evaluateDensity[A: Fractional](q: Density[A]): A = q(gram / cubic_centimetre)
  implicit def evaluateLineDensity[A: Fractional](q: LineDensity[A]): A = q(gram / centimetre)
  implicit def evaluateDynamicViscosity[A: Fractional](q: DynamicViscosity[A]): A = q(poise)
  implicit def evaluateKinematicViscosity[A: Fractional](q: KinematicViscosity[A]): A = q(stokes)
  implicit def evaluatePressure[A: Fractional](q: Pressure[A]): A = q(barye)
  implicit def evaluateVolumeFlow[A: Fractional](q: VolumeFlow[A]): A = q(cubic_centimetre / second)
  implicit def evaluateAcceleration[A: Fractional](q: Acceleration[A]): A = q(gal)
  implicit def evaluateAngularMomentum[A: Fractional](q: AngularMomentum[A]): A = q(dyne * centimetre * second)
  implicit def evaluateEnergy[A: Fractional](q: Energy[A]): A = q(erg)
  implicit def evaluateForce[A: Fractional](q: Force[A]): A = q(dyne)
  implicit def evaluateMassTorque[A: Fractional](q: MassTorque[A]): A = q(gram * centimetre)
  implicit def evaluateMomentum[A: Fractional](q: Momentum[A]): A = q(dyne * second)
  implicit def evaluatePower[A: Fractional](q: Power[A]): A = q(erg / second)
  implicit def evaluateTimeSquared[A: Fractional](q: TimeSquared[A]): A = q(second_squared)
  implicit def evaluateIrradiance[A: Fractional](q: Irradiance[A]): A = q(erg / second / square_centimetre)
  implicit def evaluateSpectralIrradiance[A: Fractional](q: SpectralIrradiance[A]): A = q(erg / second / square_centimetre / heltz)
  implicit def evaluateAbsorbedDose[A: Fractional](q: AbsorbedDose[A]): A = q(rad)
  implicit def evaluateEquivalentDose[A: Fractional](q: EquivalentDose[A]): A = q(roentgen_equivalent_man)
  implicit def evaluateEquivalentDoseRate[A: Fractional](q: EquivalentDoseRate[A]): A = q(roentgen_equivalent_man / second)
  implicit def evaluateRadioactivity[A: Fractional](q: Radioactivity[A]): A = q(curie)
}

object CGS extends CGS
