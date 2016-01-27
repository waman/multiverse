package org.waman.multiverse

import spire.math.{Real, Fractional}

trait UnitConverter[A]{

  protected val algebra: Fractional[A]
  protected def real(r: Real): A = algebra.fromReal(r)

  protected def div(a: A, u: A)     : A = algebra.div(a, u)
  protected def div(a: A, u: Double): A = div(a, algebra.fromDouble(u))
  protected def div(a: A, u: Real)  : A = div(a, algebra.fromReal(u))

  protected def times(a: A, u: A)     : A = algebra.times(a, u)
  protected def times(a: A, u: Double): A = times(a, algebra.fromDouble(u))
  protected def times(a: A, u: Real)  : A = times(a, algebra.fromReal(u))
}
