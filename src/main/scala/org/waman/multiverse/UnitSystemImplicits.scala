package org.waman.multiverse

import org.waman.multiverse.angle._
import org.waman.multiverse.time.Frequency
import spire.math.Fractional

import scala.language.implicitConversions

class Dot
class Per

trait UnitSystemImplicits{

  implicit def convertFractionalToUnitInterpreter[A: Fractional](value: A): QuantityFactory[A] =
    new QuantityFactory(value)

  implicit def convertAngularVelocityToFrequency[A: Fractional](av: AngularVelocity[A]): Frequency[A] =
    av.toFrequency

  implicit def convertFrequencyToAngularVelocity[A: Fractional](f: Frequency[A]): AngularVelocity[A] =
    f.toAngularVelocity

  val * = new Dot
  val / = new Per
}