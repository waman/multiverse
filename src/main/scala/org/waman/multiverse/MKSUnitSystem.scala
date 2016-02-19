package org.waman.multiverse

import org.waman.multiverse.angle.{Angle, AngularVelocity, SolidAngle}
import org.waman.multiverse.fluid.VolumeFlow
import org.waman.multiverse.mass.{Density, Mass}
import org.waman.multiverse.mechanics.{Acceleration, Force, Velocity}
import org.waman.multiverse.metric.{Area, Length, Volume}
import org.waman.multiverse.time.{Frequency, Time, TimeSquared}
import spire.math.Fractional

import scala.language.{implicitConversions, postfixOps}

trait MKSUnitSystem extends UnitSystem{

  implicit def convertLengthToFractional         [A: Fractional](length: Length[A])     : A = length m
  implicit def convertAreaToFractional           [A: Fractional](area: Area[A])         : A = area m2
  implicit def convertVolumeToFractional         [A: Fractional](volume: Volume[A])     : A = volume m3
  implicit def convertAngleToFractional          [A: Fractional](angle: Angle[A])       : A = angle rad
  implicit def convertSolidAngleToFractional     [A: Fractional](sa: SolidAngle[A])     : A = sa sr
  implicit def convertMassToFractional           [A: Fractional](mass: Mass[A])         : A = mass kg
  implicit def convertDensityToFractional        [A: Fractional](density: Density[A])   : A = density kg/m3
  implicit def convertTimeToFractional           [A: Fractional](time: Time[A])         : A = time s
  implicit def convertTimeSquaredToFractional    [A: Fractional](ts: TimeSquared[A])    : A = ts s2
  implicit def convertFrequencyToFractional      [A: Fractional](f: Frequency[A])       : A = f Hz
  implicit def convertVelocityToFractional       [A: Fractional](v: Velocity[A])        : A = v m/s
  implicit def convertAngularVelocityToFractional[A: Fractional](av: AngularVelocity[A]): A = av rad/s
  implicit def convertVolumeFlowToFractional     [A: Fractional](vf: VolumeFlow[A])     : A = vf m3/s
  implicit def convertAccelerationToFractional   [A: Fractional](a: Acceleration[A])    : A = a m/s2
  implicit def convertForceToFractional          [A: Fractional](f: Force[A])           : A = f N
}

object MKSUnitSystem extends MKSUnitSystem