package waman.multiverse.unitsystem

import waman.multiverse.{PhysicalUnit, Quantity}
import spire.math.Fractional
import waman.multiverse.Quantity

import scala.language.implicitConversions

//***** SI Unit System *****
trait SI extends UnitSystem{

  implicit def evaluateQuantity[A: Fractional, U <: PhysicalUnit[U]](q: Quantity[A, U]): A = q.getSIValue
}

object SI extends SI