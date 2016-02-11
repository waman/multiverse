package org.waman.multiverse

import spire.math.{Algebraic, Rational, Real}

import scala.language.postfixOps

class IntegralAsSpec extends MultiverseCustomSpec{

  "When IntegralAsFloat is imported, Integrals are converted to a Float value(java.lang.Float)" in {
    import IntegralAsFloat._
    __Exercise__
    val sut = 3 m;
    __Verify__
    sut.cm should be (a [java.lang.Float])
    sut.cm should equal (300f)
  }

  "When IntegralAsDouble is imported, Integrals are converted to a Double value(java.lang.Double)" in {
    import IntegralAsDouble._
    __Exercise__
    val sut = 3 m;
    __Verify__
    sut.cm should be (a [java.lang.Double])
    sut.cm should equal (%(300.0))
  }

  "When IntegralAsBigDecimal is imported, Integrals are converted to a BigDecimal value" in {
    import IntegralAsBigDecimal._
    __Exercise__
    val sut = 3 m;
    __Verify__
    sut.cm should be (a [BigDecimal])
    sut.cm should equal (BigDecimal(300))
  }

  "When IntegralAsRational is imported, Integrals are converted to a Rational value" in {
    import IntegralAsRational._
    __Exercise__
    val sut = 3 m;
    __Verify__
    sut.cm should be (a [Rational])
    sut.cm should equal (Rational(300))
  }

  "When IntegralAsReal is imported, Integrals are converted to a Real value" in {
    import IntegralAsReal._
    __Exercise__
    val sut = 3 m;
    __Verify__
    sut.cm should be (a [Real])
    sut.cm should equal (Real(300))
  }

  "When IntegralAsAlgebraic is imported, Integrals are converted to a Algebraic value" in {
    import IntegralAsAlgebraic._
    __Exercise__
    val sut = 3 m;
    __Verify__
    sut.cm should be (a [Algebraic])
    sut.cm should equal (Algebraic(300))
  }
}
