package org.waman.multiverse

import spire.math.{Fractional, Real, SafeLong}

import scala.language.implicitConversions

package object implicits {

  implicit def convertFractionalToQuantityFactory[A: Fractional](value: A): QuantityFactory[A] =
    new QuantityFactory(value)

  // Integral value (like 1(m), not 1.0(m)) create a Quantity[Real] instance
  implicit def convertIntToQuantityFactory(value: Int): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertLongToQuantityFactory(value: Long): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertSafeLongToQuantityFactory(value: SafeLong): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertBigIntToQuantityFactory(value: BigInt): QuantityFactory[Real] =
    new QuantityFactory(Real(value))
}
