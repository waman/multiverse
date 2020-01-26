package org.waman.multiverse.unitsystem

import org.waman.multiverse.{PhysicalUnit, Quantity}
import spire.math.Fractional

import scala.language.implicitConversions

//***** SI Unit System *****
trait SI extends UnitSystem{

  implicit def evaluateQuantity[A: Fractional, U <: PhysicalUnit[U]](q: Quantity[A, U]): A = q.getSIValue
}

object SI extends SI