package org.waman.multiverse

import spire.math.{Algebraic, Integral, Rational, Real}

import scala.language.implicitConversions

// Float
trait IntegralAsFloat{
  implicit def convertIntegralAsFloat[A: Integral](value: A): UnitInterpreter[Float] =
    new UnitInterpreter(implicitly[Integral[A]].toFloat(value))
}

object IntegralAsFloat extends IntegralAsFloat

// Double
trait IntegralAsDouble{
  implicit def convertIntegralAsDouble[A: Integral](value: A): UnitInterpreter[Double] =
    new UnitInterpreter(implicitly[Integral[A]].toDouble(value))
}

object IntegralAsDouble extends IntegralAsDouble

// BigDecimal
trait IntegralAsBigDecimal{
  implicit def convertIntegralAsBigDecimal[A: Integral](value: A): UnitInterpreter[BigDecimal] =
    new UnitInterpreter(implicitly[Integral[A]].toBigDecimal(value))
}

object IntegralAsBigDecimal extends IntegralAsBigDecimal

// Rational
trait IntegralAsRational{
  implicit def convertIntegralAsRational[A: Integral](value: A): UnitInterpreter[Rational] =
    new UnitInterpreter(implicitly[Integral[A]].toRational(value))
}

object IntegralAsRational extends IntegralAsRational

// Real
trait IntegralAsReal{
  implicit def convertIntegralAsReal[A: Integral](value: A): UnitInterpreter[Real] =
    new UnitInterpreter(implicitly[Integral[A]].toReal(value))
}

object IntegralAsReal extends IntegralAsReal

// Algebraic
trait IntegralAsAlgebraic{
  implicit def convertIntegralAsAlgebraic[A: Integral](value: A): UnitInterpreter[Algebraic] =
    new UnitInterpreter(implicitly[Integral[A]].toAlgebraic(value))
}

object IntegralAsAlgebraic extends IntegralAsAlgebraic