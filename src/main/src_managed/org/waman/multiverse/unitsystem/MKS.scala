package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.angle._
import org.waman.multiverse.unit.defs.density._
import org.waman.multiverse.unit.defs.fluid._
import org.waman.multiverse.unit.defs.mechanics._
import org.waman.multiverse.unit.defs.radiometry._
import org.waman.multiverse.unit.defs.radioactivity._

/** Evaluate quantities by metre, kilogram, and second. */
trait MKS extends UnitSystem {
  implicit def evaluateAngle[A: Fractional](q: Angle[A]): A = q(AngleUnitObjects.radian)
  implicit def evaluateAngularVelocity[A: Fractional](q: AngularVelocity[A]): A = q(AngleUnitObjects.radian / TimeUnitObjects.second)
  implicit def evaluateFrequency[A: Fractional](q: Frequency[A]): A = q(FrequencyUnitObjects.heltz)
  implicit def evaluateSolidAngle[A: Fractional](q: SolidAngle[A]): A = q(SolidAngleUnitObjects.steradian)
  implicit def evaluateArea[A: Fractional](q: Area[A]): A = q(AreaUnitObjects.square_metre)
  implicit def evaluateLength[A: Fractional](q: Length[A]): A = q(LengthUnitObjects.metre)
  implicit def evaluateMass[A: Fractional](q: Mass[A]): A = q(MassUnitObjects.kilogram)
  implicit def evaluateTime[A: Fractional](q: Time[A]): A = q(TimeUnitObjects.second)
  implicit def evaluateVelocity[A: Fractional](q: Velocity[A]): A = q(LengthUnitObjects.metre / TimeUnitObjects.second)
  implicit def evaluateVolume[A: Fractional](q: Volume[A]): A = q(VolumeUnitObjects.cubic_metre)
  implicit def evaluateDensity[A: Fractional](q: Density[A]): A = q(MassUnitObjects.kilogram / VolumeUnitObjects.cubic_metre)
  implicit def evaluateLineDensity[A: Fractional](q: LineDensity[A]): A = q(MassUnitObjects.kilogram / LengthUnitObjects.metre)
  implicit def evaluateDynamicViscosity[A: Fractional](q: DynamicViscosity[A]): A = q(PressureUnitObjects.pascal * TimeUnitObjects.second)
  implicit def evaluateKinematicViscosity[A: Fractional](q: KinematicViscosity[A]): A = q(AreaUnitObjects.square_metre / TimeUnitObjects.second)
  implicit def evaluatePressure[A: Fractional](q: Pressure[A]): A = q(PressureUnitObjects.pascal)
  implicit def evaluateVolumeFlow[A: Fractional](q: VolumeFlow[A]): A = q(VolumeUnitObjects.cubic_metre / TimeUnitObjects.second)
  implicit def evaluateAcceleration[A: Fractional](q: Acceleration[A]): A = q(LengthUnitObjects.metre / TimeSquaredUnitObjects.second_squared)
  implicit def evaluateAngularMomentum[A: Fractional](q: AngularMomentum[A]): A = q(ForceUnitObjects.newton * LengthUnitObjects.metre * TimeUnitObjects.second)
  implicit def evaluateEnergy[A: Fractional](q: Energy[A]): A = q(EnergyUnitObjects.joule)
  implicit def evaluateForce[A: Fractional](q: Force[A]): A = q(ForceUnitObjects.newton)
  implicit def evaluateMassTorque[A: Fractional](q: MassTorque[A]): A = q(MassUnitObjects.kilogram * LengthUnitObjects.metre)
  implicit def evaluateMomentum[A: Fractional](q: Momentum[A]): A = q(ForceUnitObjects.newton * TimeUnitObjects.second)
  implicit def evaluatePower[A: Fractional](q: Power[A]): A = q(PowerUnitObjects.watt)
  implicit def evaluateTimeSquared[A: Fractional](q: TimeSquared[A]): A = q(TimeSquaredUnitObjects.second_squared)
  implicit def evaluateIrradiance[A: Fractional](q: Irradiance[A]): A = q(PowerUnitObjects.watt / AreaUnitObjects.square_metre)
  implicit def evaluateSpectralIrradiance[A: Fractional](q: SpectralIrradiance[A]): A = q(PowerUnitObjects.watt / AreaUnitObjects.square_metre / FrequencyUnitObjects.heltz)
  implicit def evaluateAbsorbedDose[A: Fractional](q: AbsorbedDose[A]): A = q(AbsorbedDoseUnitObjects.gray)
  implicit def evaluateEquivalentDose[A: Fractional](q: EquivalentDose[A]): A = q(EquivalentDoseUnitObjects.sievert)
  implicit def evaluateEquivalentDoseRate[A: Fractional](q: EquivalentDoseRate[A]): A = q(EquivalentDoseUnitObjects.sievert / TimeUnitObjects.second)
  implicit def evaluateRadioactivity[A: Fractional](q: Radioactivity[A]): A = q(RadioactivityUnitObjects.becquerel)
}

object MKS extends MKS
