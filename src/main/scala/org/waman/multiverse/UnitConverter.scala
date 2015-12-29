package org.waman.multiverse

import spire.math.{Real, Fractional}

trait UnitConverter[A]{

  protected val algebra: Fractional[A]
  protected def real(r: Real): A = algebra.fromReal(r)

  protected def convertTo(a: A, u: A): A = algebra.div(a, u)
  protected def convertTo(a: A, u: Double): A = convertTo(a, algebra.fromDouble(u))
  protected def convertTo(a: A, u: Real): A = convertTo(a, algebra.fromReal(u))

  protected def convertFrom(a: A, u: A): A = algebra.times(a, u)
  protected def convertFrom(a: A, u: Double): A = convertFrom(a, algebra.fromDouble(u))
  protected def convertFrom(a: A, u: Real): A = convertFrom(a, algebra.fromReal(u))
}
