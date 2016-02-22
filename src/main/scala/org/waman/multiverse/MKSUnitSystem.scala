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
import org.waman.multiverse.time.{Frequency, Time, TimeSquared}
import spire.math.Fractional

import scala.language.{implicitConversions, postfixOps}

trait MKSUnitSystem extends UnitSystem{

  implicit def convertLengthToFractional            [A: Fractional](l: Length[A])             : A = l.m
  implicit def convertAreaToFractional              [A: Fractional](a: Area[A])               : A = a.m2
  implicit def convertVolumeToFractional            [A: Fractional](v: Volume[A])             : A = v.m3
  implicit def convertAngleToFractional             [A: Fractional](a: Angle[A])              : A = a.rad
  implicit def convertSolidAngleToFractional        [A: Fractional](sa: SolidAngle[A])        : A = sa.sr
  implicit def convertMassToFractional              [A: Fractional](m: Mass[A])               : A = m.kg
  implicit def convertDensityToFractional           [A: Fractional](d: Density[A])            : A = d.kg/m3
  implicit def convertTimeToFractional              [A: Fractional](t: Time[A])               : A = t.s
  implicit def convertTimeSquaredToFractional       [A: Fractional](ts: TimeSquared[A])       : A = ts.s2
  implicit def convertFrequencyToFractional         [A: Fractional](f: Frequency[A])          : A = f.Hz

  implicit def convertVelocityToFractional          [A: Fractional](v: Velocity[A])           : A = v.m/s
  implicit def convertAngularVelocityToFractional   [A: Fractional](av: AngularVelocity[A])   : A = av.rad/s
  implicit def convertVolumeFlowToFractional        [A: Fractional](vf: VolumeFlow[A])        : A = vf.m3/s
  implicit def convertAccelerationToFractional      [A: Fractional](a: Acceleration[A])       : A = a.m/s2
  implicit def convertForceToFractional             [A: Fractional](f: Force[A])              : A = f.N
  implicit def convertPressureToFractional          [A: Fractional](p: Pressure[A])           : A = p.Pa
  implicit def convertTorqueToFractional            [A: Fractional](t: Torque[A])             : A = t.N*m
  implicit def convertEnergyToFractional            [A: Fractional](e: Energy[A])             : A = e.J
  implicit def convertPowerToFractional             [A: Fractional](p: Power[A])              : A = p.W
  implicit def convertActionToFractional            [A: Fractional](a: Action[A])             : A = a.ħ

  implicit def convertDynamicViscosityToFractional  [A: Fractional](dv: DynamicViscosity[A])  : A = dv.Pa*s
  implicit def convertKinematicViscosityToFractional[A: Fractional](kv: KinematicViscosity[A]): A = kv.m2/s
  implicit def convertCurrentToFractional           [A: Fractional](c: Current[A])            : A = c.A
  implicit def convertChargeToFractional            [A: Fractional](c: Charge[A])             : A = c.C
  implicit def convertDipoleToFractional            [A: Fractional](d: Dipole[A])             : A = d.C*m
  implicit def convertVoltageToFractional           [A: Fractional](v: Voltage[A])            : A = v.V
  implicit def convertResistanceToFractional        [A: Fractional](r: Resistance[A])         : A = r.Ω
  implicit def convertCapacitanceToFractional       [A: Fractional](c: Capacitance[A])        : A = c.F
  implicit def convertFluxToFractional              [A: Fractional](f: Flux[A])               : A = f.Wb
  implicit def convertFluxDensityToFractional       [A: Fractional](fd: FluxDensity[A])       : A = fd.T

  implicit def convertInductanceToFractional        [A: Fractional](i: Inductance[A])         : A = i.H
  implicit def convertLuminousIntensityToFractional [A: Fractional](li: LuminousIntensity[A]) : A = li.cd
  implicit def convertLuminanceToFractional         [A: Fractional](l: Luminance[A])          : A = l.cd/m2
  implicit def convertLuminousFluxToFractional      [A: Fractional](lf: LuminousFlux[A])      : A = lf.lm
  implicit def convertIlluminanceToFractional       [A: Fractional](i: Illuminance[A])        : A = i.lx
}

object MKSUnitSystem extends MKSUnitSystem