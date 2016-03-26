package org.waman.multiverse

import spire.math._

import scala.language.implicitConversions

trait ConvertableFromIntegral{

  trait IntegralConverter[A]{
    def convertToFloat     (value: A): Float
    def convertToDouble    (value: A): Double
    def convertToBigDecimal(value: A): BigDecimal
    def convertToRational  (value: A): Rational
    def convertToReal      (value: A): Real
    def convertToAlgebraic (value: A): Algebraic
  }

  implicit object ConvertableFromByte extends IntegralConverter[Byte]{
    override def convertToFloat     (value: Byte): Float      = value.toFloat
    override def convertToDouble    (value: Byte): Double     = value.toDouble
    override def convertToBigDecimal(value: Byte): BigDecimal = BigDecimal(value)
    override def convertToRational  (value: Byte): Rational   = Rational(value)
    override def convertToReal      (value: Byte): Real       = Real(value)
    override def convertToAlgebraic (value: Byte): Algebraic  = Algebraic(value)
  }

  implicit object ConvertableFromShort extends IntegralConverter[Short]{
    override def convertToFloat     (value: Short): Float      = value.toFloat
    override def convertToDouble    (value: Short): Double     = value.toDouble
    override def convertToBigDecimal(value: Short): BigDecimal = BigDecimal(value)
    override def convertToRational  (value: Short): Rational   = Rational(value)
    override def convertToReal      (value: Short): Real       = Real(value)
    override def convertToAlgebraic (value: Short): Algebraic  = Algebraic(value)
  }

  implicit object ConvertableFromInt extends IntegralConverter[Int]{
    override def convertToFloat     (value: Int): Float      = value.toFloat
    override def convertToDouble    (value: Int): Double     = value.toDouble
    override def convertToBigDecimal(value: Int): BigDecimal = BigDecimal(value)
    override def convertToRational  (value: Int): Rational   = Rational(value)
    override def convertToReal      (value: Int): Real       = Real(value)
    override def convertToAlgebraic (value: Int): Algebraic  = Algebraic(value)
  }

  implicit object ConvertableFromLong extends IntegralConverter[Long]{
    override def convertToFloat     (value: Long): Float      = value.toFloat
    override def convertToDouble    (value: Long): Double     = value.toDouble
    override def convertToBigDecimal(value: Long): BigDecimal = BigDecimal(value)
    override def convertToRational  (value: Long): Rational   = Rational(value)
    override def convertToReal      (value: Long): Real       = Real(value)
    override def convertToAlgebraic (value: Long): Algebraic  = Algebraic(value)
  }

  implicit object ConvertableFromSafeLong extends IntegralConverter[SafeLong]{
    override def convertToFloat     (value: SafeLong): Float      = value.toFloat
    override def convertToDouble    (value: SafeLong): Double     = value.toDouble
    override def convertToBigDecimal(value: SafeLong): BigDecimal = value.toBigDecimal
    override def convertToRational  (value: SafeLong): Rational   = Rational(value)
    override def convertToReal      (value: SafeLong): Real       = Real(value)
    override def convertToAlgebraic (value: SafeLong): Algebraic  = Algebraic(value)
  }

  implicit object ConvertableFromBigInt extends IntegralConverter[BigInt]{
    override def convertToFloat     (value: BigInt): Float      = value.toFloat
    override def convertToDouble    (value: BigInt): Double     = value.toDouble
    override def convertToBigDecimal(value: BigInt): BigDecimal = BigDecimal(value)
    override def convertToRational  (value: BigInt): Rational   = Rational(value)
    override def convertToReal      (value: BigInt): Real       = Real(value)
    override def convertToAlgebraic (value: BigInt): Algebraic  = Algebraic(value)
  }
}

// Float
trait IntegralAsFloat extends ConvertableFromIntegral{

//  implicit def convertIntegralToFloat[A: Integral](value: A): Float =
//    implicitly[Integral[A]].toFloat(value)
//
//  implicit def convertIntegralToQuantityFactoryAsFloat[A: Fractional](value: A): QuantityFactory[Float] =
//    new QuantityFactory(convertIntegralToFloat(value))

  implicit def convertIntegralToFloat[A](value: A)(implicit ic: IntegralConverter[A]): Float =
    ic.convertToFloat(value)

  implicit def convertIntegralToQuantityFactoryAsFloat[A](value: A)(implicit ic: IntegralConverter[A]):
      QuantityFactory[Float] =
    new QuantityFactory(ic.convertToFloat(value))
}

object IntegralAsFloat extends IntegralAsFloat

// Double
trait IntegralAsDouble extends ConvertableFromIntegral{
//  implicit def convertIntegralToDouble[A: Integral](value: A): Double =
//    implicitly[Integral[A]].toDouble(value)
//
//  implicit def convertIntegralToQuantityFactoryAsDouble[A: Integral](value: A): QuantityFactory[Double] =
//    new QuantityFactory(convertIntegralToDouble(value))

  implicit def convertIntegralToDouble[A](value: A)(implicit ic: IntegralConverter[A]): Double =
    ic.convertToDouble(value)

  implicit def convertIntegralToQuantityFactoryAsDouble[A](value: A)(implicit ic: IntegralConverter[A]):
      QuantityFactory[Double] =
    new QuantityFactory(ic.convertToDouble(value))
}

object IntegralAsDouble extends IntegralAsDouble

// BigDecimal
trait IntegralAsBigDecimal extends ConvertableFromIntegral{
//  implicit def convertIntegralToBigDecimal[A: Integral](value: A): BigDecimal =
//    implicitly[Integral[A]].toBigDecimal(value)
//
//  implicit def convertIntegralToQuantityFactoryAsBigDecimal[A: Integral](value: A): QuantityFactory[BigDecimal] =
//    new QuantityFactory(convertIntegralToBigDecimal(value))

  implicit def convertIntegralToBigDecimal[A](value: A)(implicit ic: IntegralConverter[A]): BigDecimal =
    ic.convertToBigDecimal(value)

  implicit def convertIntegralToQuantityFactoryAsBigDecimal[A](value: A)(implicit ic: IntegralConverter[A]):
      QuantityFactory[BigDecimal] =
    new QuantityFactory(ic.convertToBigDecimal(value))
}

object IntegralAsBigDecimal extends IntegralAsBigDecimal

// Rational
trait IntegralAsRational extends ConvertableFromIntegral{
//  implicit def convertIntegralToRational[A: Integral](value: A): Rational =
//    implicitly[Integral[A]].toRational(value)
//
//  implicit def convertIntegralToQuantityFactoryAsRational[A: Integral](value: A): QuantityFactory[Rational] =
//    new QuantityFactory(convertIntegralToRational(value))

  implicit def convertIntegralToRational[A](value: A)(implicit ic: IntegralConverter[A]): Rational =
    ic.convertToRational(value)

  implicit def convertIntegralToQuantityFactoryAsRational[A](value: A)(implicit ic: IntegralConverter[A]):
      QuantityFactory[Rational] =
    new QuantityFactory(ic.convertToRational(value))
}

object IntegralAsRational extends IntegralAsRational

// Real
trait IntegralAsReal extends ConvertableFromIntegral{
//  implicit def convertIntegralToReal[A: Integral](value: A): Real =
//    implicitly[Integral[A]].toReal(value)
//
//  implicit def convertIntegralToQuantityFactoryAsReal[A: Integral](value: A): QuantityFactory[Real] =
//    new QuantityFactory(convertIntegralToReal(value))

  implicit def convertIntegralToReal[A](value: A)(implicit ic: IntegralConverter[A]): Real =
    ic.convertToReal(value)

  implicit def convertIntegralToQuantityFactoryAsReal[A](value: A)(implicit ic: IntegralConverter[A]):
      QuantityFactory[Real] =
    new QuantityFactory(ic.convertToReal(value))
}

object IntegralAsReal extends IntegralAsReal

// Algebraic
trait IntegralAsAlgebraic extends ConvertableFromIntegral{
//  implicit def convertIntegralToAlgebraic[A: Integral](value: A): Algebraic =
//    implicitly[Integral[A]].toAlgebraic(value)
//
//  implicit def convertIntegralToQuantityFactoryAsAlgebraic[A: Integral](value: A): QuantityFactory[Algebraic] =
//    new QuantityFactory(convertIntegralToAlgebraic(value))

  implicit def convertIntegralToAlgebraic[A](value: A)(implicit ic: IntegralConverter[A]): Algebraic =
    ic.convertToAlgebraic(value)

  implicit def convertIntegralToQuantityFactoryAsAlgebraic[A](value: A)(implicit ic: IntegralConverter[A]):
      QuantityFactory[Algebraic] =
    new QuantityFactory(ic.convertToAlgebraic(value))
}

object IntegralAsAlgebraic extends IntegralAsAlgebraic