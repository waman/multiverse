package waman.multiverse.unitsystem

import scala.language.implicitConversions

import waman.multiverse.unit.angle.Angle
import waman.multiverse.unit.angle.AngularVelocity
import waman.multiverse.unit.angle.Frequency
import waman.multiverse.unit.angle.SolidAngle
import waman.multiverse.unit.basic.Area
import waman.multiverse.unit.basic.Length
import waman.multiverse.unit.basic.Mass
import waman.multiverse.unit.basic.Time
import waman.multiverse.unit.basic.Velocity
import waman.multiverse.unit.basic.Volume
import waman.multiverse.unit.density.Density
import waman.multiverse.unit.density.LineDensity
import waman.multiverse.unit.fluid.DynamicViscosity
import waman.multiverse.unit.fluid.KinematicViscosity
import waman.multiverse.unit.fluid.Pressure
import waman.multiverse.unit.fluid.VolumeFlow
import waman.multiverse.unit.mechanics.Acceleration
import waman.multiverse.unit.mechanics.AngularMomentum
import waman.multiverse.unit.mechanics.Energy
import waman.multiverse.unit.mechanics.Force
import waman.multiverse.unit.mechanics.MassTorque
import waman.multiverse.unit.mechanics.Momentum
import waman.multiverse.unit.mechanics.Power
import waman.multiverse.unit.mechanics.TimeSquared
import waman.multiverse.unit.radiometry.Irradiance
import waman.multiverse.unit.radiometry.AreaFrequency
import waman.multiverse.unit.radiometry.SpectralIrradiance
import waman.multiverse.unit.radioactivity.AbsorbedDose
import waman.multiverse.unit.radioactivity.EquivalentDose
import waman.multiverse.unit.radioactivity.EquivalentDoseRate
import waman.multiverse.unit.radioactivity.Radioactivity

import waman.multiverse.unit.angle.AngleUnitObjects.radian
import waman.multiverse.unit.basic.TimeUnitObjects.second
import waman.multiverse.unit.angle.FrequencyUnitObjects.heltz
import waman.multiverse.unit.angle.SolidAngleUnitObjects.steradian
import waman.multiverse.unit.basic.AreaUnitObjects.square_centimetre
import waman.multiverse.unit.basic.LengthUnitObjects.centimetre
import waman.multiverse.unit.basic.MassUnitObjects.gram
import waman.multiverse.unit.basic.VolumeUnitObjects.cubic_centimetre
import waman.multiverse.unit.fluid.DynamicViscosityUnitObjects.poise
import waman.multiverse.unit.fluid.KinematicViscosityUnitObjects.stokes
import waman.multiverse.unit.fluid.PressureUnitObjects.barye
import waman.multiverse.unit.mechanics.AccelerationUnitObjects.gal
import waman.multiverse.unit.mechanics.ForceUnitObjects.dyne
import waman.multiverse.unit.mechanics.EnergyUnitObjects.erg
import waman.multiverse.unit.mechanics.TimeSquaredUnitObjects.second_squared
import waman.multiverse.unit.radioactivity.AbsorbedDoseUnitObjects.rad
import waman.multiverse.unit.radioactivity.EquivalentDoseUnitObjects.roentgen_equivalent_man
import waman.multiverse.unit.radioactivity.RadioactivityUnitObjects.curie

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
  implicit def evaluateAreaFrequency[A: Fractional](q: AreaFrequency[A]): A = q(square_centimetre * heltz)
  implicit def evaluateSpectralIrradiance[A: Fractional](q: SpectralIrradiance[A]): A = q(erg / second / square_centimetre / heltz)
  implicit def evaluateAbsorbedDose[A: Fractional](q: AbsorbedDose[A]): A = q(rad)
  implicit def evaluateEquivalentDose[A: Fractional](q: EquivalentDose[A]): A = q(roentgen_equivalent_man)
  implicit def evaluateEquivalentDoseRate[A: Fractional](q: EquivalentDoseRate[A]): A = q(roentgen_equivalent_man / second)
  implicit def evaluateRadioactivity[A: Fractional](q: Radioactivity[A]): A = q(curie)
}

object CGS extends CGS
