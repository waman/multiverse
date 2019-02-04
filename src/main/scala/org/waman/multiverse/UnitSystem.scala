package org.waman.multiverse

import spire.math.Fractional
import org.waman.multiverse.units.Length
import org.waman.multiverse.predef.LengthUnits._

import scala.language.implicitConversions

trait UnitSystem

//***** MKS Unit System *****
trait MKSUnitSystem extends UnitSystem{

  implicit def convertLengthToFractional[A: Fractional](q: Length[A]): A = q(m)
}

object MKSUnitSystem extends MKSUnitSystem

//***** MKSA Unit System *****
trait MKSAUnitSystem extends MKSUnitSystem

object MKSAUnitSystem extends MKSAUnitSystem

//***** CGS Unit System *****
trait CGMUnitSystem extends UnitSystem

object CGMUnitSystem extends CGMUnitSystem
