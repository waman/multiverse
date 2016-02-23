package org.waman.multiverse

import org.waman.multiverse.angle.{Angle, AngularVelocity, SolidAngle}
import org.waman.multiverse.electric._
import org.waman.multiverse.energy.{Action, Energy, Power}
import org.waman.multiverse.fluid.{DynamicViscosity, KinematicViscosity, Pressure, VolumeFlow}
import org.waman.multiverse.luminous.{Illuminance, Luminance, LuminousFlux, LuminousIntensity}
import org.waman.multiverse.magnetic.{Flux, FluxDensity, Inductance}
import org.waman.multiverse.mass.{Density, Mass}
import org.waman.multiverse.mechanics.{Acceleration, Force, Torque, Velocity}
import org.waman.multiverse.metric.{Area, Length, Volume}
import org.waman.multiverse.radiation._
import org.waman.multiverse.thermal.{Entropy, Temperature}
import org.waman.multiverse.time.{Frequency, Time, TimeSquared}
import spire.math.Fractional

import scala.language.{implicitConversions, postfixOps}

trait MKSUnitSystem extends UnitSystem{

  implicit def convertLengthToFractional            [A: Fractional](q: Length[A])            : A = q.m
  implicit def convertAreaToFractional              [A: Fractional](q: Area[A])              : A = q.m2
  implicit def convertVolumeToFractional            [A: Fractional](q: Volume[A])            : A = q.m3
  implicit def convertAngleToFractional             [A: Fractional](q: Angle[A])             : A = q.rad
  implicit def convertSolidAngleToFractional        [A: Fractional](q: SolidAngle[A])        : A = q.sr
  implicit def convertMassToFractional              [A: Fractional](q: Mass[A])              : A = q.kg
  implicit def convertDensityToFractional           [A: Fractional](q: Density[A])           : A = q.kg/m3
  implicit def convertTimeToFractional              [A: Fractional](q: Time[A])              : A = q.s
  implicit def convertTimeSquaredToFractional       [A: Fractional](q: TimeSquared[A])       : A = q.s2
  implicit def convertFrequencyToFractional         [A: Fractional](q: Frequency[A])         : A = q.Hz

  implicit def convertVelocityToFractional          [A: Fractional](q: Velocity[A])          : A = q.m/s
  implicit def convertAngularVelocityToFractional   [A: Fractional](q: AngularVelocity[A])   : A = q.rad/s
  implicit def convertVolumeFlowToFractional        [A: Fractional](q: VolumeFlow[A])        : A = q.m3/s
  implicit def convertAccelerationToFractional      [A: Fractional](q: Acceleration[A])      : A = q.m/s2
  implicit def convertForceToFractional             [A: Fractional](q: Force[A])             : A = q.N
  implicit def convertPressureToFractional          [A: Fractional](q: Pressure[A])          : A = q.Pa
  implicit def convertTorqueToFractional            [A: Fractional](q: Torque[A])            : A = q.N*m
  implicit def convertEnergyToFractional            [A: Fractional](q: Energy[A])            : A = q.J
  implicit def convertPowerToFractional             [A: Fractional](q: Power[A])             : A = q.W
  implicit def convertActionToFractional            [A: Fractional](q: Action[A])            : A = q.ħ

  implicit def convertDynamicViscosityToFractional  [A: Fractional](q: DynamicViscosity[A])  : A = q.Pa*s
  implicit def convertKinematicViscosityToFractional[A: Fractional](q: KinematicViscosity[A]): A = q.m2/s
  implicit def convertCurrentToFractional           [A: Fractional](q: Current[A])           : A = q.A
  implicit def convertChargeToFractional            [A: Fractional](q: Charge[A])            : A = q.C
  implicit def convertDipoleToFractional            [A: Fractional](q: Dipole[A])            : A = q.C*m
  implicit def convertVoltageToFractional           [A: Fractional](q: Voltage[A])           : A = q.V
  implicit def convertResistanceToFractional        [A: Fractional](q: Resistance[A])        : A = q.Ω
  implicit def convertCapacitanceToFractional       [A: Fractional](q: Capacitance[A])       : A = q.F
  implicit def convertFluxToFractional              [A: Fractional](q: Flux[A])              : A = q.Wb
  implicit def convertFluxDensityToFractional       [A: Fractional](q: FluxDensity[A])       : A = q.T

  implicit def convertInductanceToFractional        [A: Fractional](q: Inductance[A])        : A = q.H
  implicit def convertTemperatureToFractional       [A: Fractional](q: Temperature[A])       : A = q.K
  implicit def convertEntropyToFractional           [A: Fractional](q: Entropy[A])           : A = q.J/K
  implicit def convertLuminousIntensityToFractional [A: Fractional](q: LuminousIntensity[A]) : A = q.cd
  implicit def convertLuminanceToFractional         [A: Fractional](q: Luminance[A])         : A = q.cd/m2
  implicit def convertLuminousFluxToFractional      [A: Fractional](q: LuminousFlux[A])      : A = q.lm
  implicit def convertIlluminanceToFractional       [A: Fractional](q: Illuminance[A])       : A = q.lx
  implicit def convertRadioactivityToFractional     [A: Fractional](q: Radioactivity[A])     : A = q.Bq
  implicit def convertExposureToFractional          [A: Fractional](q: Exposure[A])          : A = q.C/kg
  implicit def convertAbsorbedDoseToFractional      [A: Fractional](q: AbsorbedDose[A])      : A = q.Gy

  implicit def convertEquivalentDoseToFractional    [A: Fractional](q: EquivalentDose[A])    : A = q.Sv
  implicit def convertEquivalentDoseRateToFractional[A: Fractional](q: EquivalentDoseRate[A]): A = q.Sv/s
}

object MKSUnitSystem extends MKSUnitSystem