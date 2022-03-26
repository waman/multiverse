package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.angle._
import org.waman.multiverse.unit.defs.dens._
import org.waman.multiverse.unit.defs.fluid._
import org.waman.multiverse.unit.defs.mech._
import org.waman.multiverse.unit.defs.radio._
import org.waman.multiverse.unit.defs.radio.freq._
import org.waman.multiverse.unit.defs.ra._


trait CGS extends UnitSystem {
  implicit def evaluateAngle[A: Fractional](q: Angle[A]): A = q(AngleUnitObjects.radian)
  implicit def evaluateAngularVelocity[A: Fractional](q: AngularVelocity[A]): A = q(AngleUnitObjects.radian / TimeUnitObjects.second)
  implicit def evaluateFrequency[A: Fractional](q: Frequency[A]): A = q(FrequencyUnitObjects.heltz)
  implicit def evaluateSolidAngle[A: Fractional](q: SolidAngle[A]): A = q(SolidAngleUnitObjects.steradian)
  implicit def evaluateArea[A: Fractional](q: Area[A]): A = q(AreaUnitObjects.square_centimetre)
  implicit def evaluateLength[A: Fractional](q: Length[A]): A = q(LengthUnitObjects.centimetre)
  implicit def evaluateMass[A: Fractional](q: Mass[A]): A = q(MassUnitObjects.gram)
  implicit def evaluateTime[A: Fractional](q: Time[A]): A = q(TimeUnitObjects.second)
  implicit def evaluateVelocity[A: Fractional](q: Velocity[A]): A = q(LengthUnitObjects.centimetre / TimeUnitObjects.second)
  implicit def evaluateVolume[A: Fractional](q: Volume[A]): A = q(VolumeUnitObjects.cubic_centimetre)
  implicit def evaluateDensity[A: Fractional](q: Density[A]): A = q(MassUnitObjects.gram / VolumeUnitObjects.cubic_centimetre)
  implicit def evaluateLineDensity[A: Fractional](q: LineDensity[A]): A = q(MassUnitObjects.gram / LengthUnitObjects.centimetre)
  implicit def evaluateDynamicViscosity[A: Fractional](q: DynamicViscosity[A]): A = q(DynamicViscosityUnitObjects.poise)
  implicit def evaluateKinematicViscosity[A: Fractional](q: KinematicViscosity[A]): A = q(KinematicViscosityUnitObjects.stokes)
  implicit def evaluatePressure[A: Fractional](q: Pressure[A]): A = q(PressureUnitObjects.barye)
  implicit def evaluateVolumeFlow[A: Fractional](q: VolumeFlow[A]): A = q(VolumeUnitObjects.cubic_centimetre / TimeUnitObjects.second)
  implicit def evaluateAcceleration[A: Fractional](q: Acceleration[A]): A = q(AccelerationUnitObjects.gal)
  implicit def evaluateAngularMomentum[A: Fractional](q: AngularMomentum[A]): A = q(ForceUnitObjects.dyne * LengthUnitObjects.centimetre * TimeUnitObjects.second)
  implicit def evaluateEnergy[A: Fractional](q: Energy[A]): A = q(EnergyUnitObjects.erg)
  implicit def evaluateForce[A: Fractional](q: Force[A]): A = q(ForceUnitObjects.dyne)
  implicit def evaluateMassTorque[A: Fractional](q: MassTorque[A]): A = q(MassUnitObjects.gram * LengthUnitObjects.centimetre)
  implicit def evaluateMomentum[A: Fractional](q: Momentum[A]): A = q(ForceUnitObjects.dyne * TimeUnitObjects.second)
  implicit def evaluatePower[A: Fractional](q: Power[A]): A = q(EnergyUnitObjects.erg / TimeUnitObjects.second)
  implicit def evaluateTimeSquared[A: Fractional](q: TimeSquared[A]): A = q(TimeSquaredUnitObjects.second_squared)
  implicit def evaluateIrradiance[A: Fractional](q: Irradiance[A]): A = q(EnergyUnitObjects.erg / TimeUnitObjects.second / AreaUnitObjects.square_centimetre)
  implicit def evaluateSpectralIrradiance[A: Fractional](q: SpectralIrradiance[A]): A = q(EnergyUnitObjects.erg / TimeUnitObjects.second / AreaUnitObjects.square_centimetre / FrequencyUnitObjects.heltz)
  implicit def evaluateAbsorbedDose[A: Fractional](q: AbsorbedDose[A]): A = q(AbsorbedDoseUnitObjects.rad)
  implicit def evaluateEquivalentDose[A: Fractional](q: EquivalentDose[A]): A = q(EquivalentDoseUnitObjects.roentgen_equivalent_man)
  implicit def evaluateEquivalentDoseRate[A: Fractional](q: EquivalentDoseRate[A]): A = q(EquivalentDoseUnitObjects.roentgen_equivalent_man / TimeUnitObjects.second)
  implicit def evaluateRadioactivity[A: Fractional](q: Radioactivity[A]): A = q(RadioactivityUnitObjects.curie)
}

object CGS extends CGS
