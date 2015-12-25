package org.waman.multiverse

import scala.language.implicitConversions

trait MKSUnitSystem extends UnitSystem{
  implicit def convertLengthToDouble(length: Length): Double = length.m
  implicit def convertTimeToDouble(time: Time): Double = time.s
  implicit def convertVelocityToDouble(v: Velocity): Double = v.`m/s`
}

object MKSUnitSystem extends MKSUnitSystem