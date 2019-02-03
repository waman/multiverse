package org.waman.multiverse

import spire.math.Fractional
import org.waman.multiverse.metric.Length
import org.waman.multiverse.predef.LengthUnits._

import scala.language.implicitConversions

trait MKSUnitSystem extends UnitSystem{

  implicit def convertLengthToFractional[A: Fractional](q: Length[A]): A = q(m)
}

object MKSUnitSystem extends MKSUnitSystem