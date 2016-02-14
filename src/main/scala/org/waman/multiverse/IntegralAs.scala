package org.waman.multiverse

import spire.math.{Algebraic, Integral, Rational, Real}

import scala.language.implicitConversions

// Float
trait IntegralAsFloat{
  implicit def convertIntegralToFloat[A: Integral](value: A): Float =
    implicitly[Integral[A]].toFloat(value)

  implicit def convertIntegralToUnitInterpreterAsFloat[A: Integral](value: A): UnitInterpreter[Float] =
    new UnitInterpreter(implicitly[Integral[A]].toFloat(value))
}

object IntegralAsFloat extends IntegralAsFloat

// Double
trait IntegralAsDouble{
  implicit def convertIntegralToDouble[A: Integral](value: A): Double =
    implicitly[Integral[A]].toDouble(value)

  implicit def convertIntegralToUnitInterpreterAsDouble[A: Integral](value: A): UnitInterpreter[Double] =
    new UnitInterpreter(convertIntegralToDouble(value))
}

object IntegralAsDouble extends IntegralAsDouble

// BigDecimal
trait IntegralAsBigDecimal{
  implicit def convertIntegralToBigDecimal[A: Integral](value: A): BigDecimal =
    implicitly[Integral[A]].toBigDecimal(value)

  implicit def convertIntegralToUnitInterpreterAsBigDecimal[A: Integral](value: A): UnitInterpreter[BigDecimal] =
    new UnitInterpreter(convertIntegralToBigDecimal(value))
}

object IntegralAsBigDecimal extends IntegralAsBigDecimal

// Rational
trait IntegralAsRational{
  implicit def convertIntegralToRational[A: Integral](value: A): Rational =
    implicitly[Integral[A]].toRational(value)

  implicit def convertIntegralToUnitInterpreterAsRational[A: Integral](value: A): UnitInterpreter[Rational] =
    new UnitInterpreter(convertIntegralToRational(value))
}

object IntegralAsRational extends IntegralAsRational

// Real
trait IntegralAsReal{
  implicit def convertIntegralToReal[A: Integral](value: A): Real =
    implicitly[Integral[A]].toReal(value)

  implicit def convertIntegralToUnitInterpreterAsReal[A: Integral](value: A): UnitInterpreter[Real] =
    new UnitInterpreter(convertIntegralToReal(value))
}

object IntegralAsReal extends IntegralAsReal

// Algebraic
trait IntegralAsAlgebraic{
  implicit def convertIntegralToAlgebraic[A: Integral](value: A): Algebraic =
    implicitly[Integral[A]].toAlgebraic(value)

  implicit def convertIntegralToUnitInterpreterAsAlgebraic[A: Integral](value: A): UnitInterpreter[Algebraic] =
    new UnitInterpreter(convertIntegralToAlgebraic(value))
}

object IntegralAsAlgebraic extends IntegralAsAlgebraic