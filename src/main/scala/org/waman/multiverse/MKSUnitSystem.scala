package org.waman.multiverse

import spire.math.Fractional
import scala.language.implicitConversions

trait MKSUnitSystem extends UnitSystem{
  implicit def convertLengthToFractional[A: Fractional](length: Length[A]): A = length.m
  implicit def convertTimeToFractional[A: Fractional](time: Time[A]): A = time.s
  implicit def convertVelocityToFractional[A: Fractional](v: Velocity[A]): A = v.`m/s`

  implicit def convertAngleToFractional[A: Fractional](angle: Angle[A]): A = angle.rad
  implicit def convertAngularVelocityToFractional[A: Fractional](av: AngularVelocity[A]): A = av.`rad/s`
}

object MKSUnitSystem extends MKSUnitSystem