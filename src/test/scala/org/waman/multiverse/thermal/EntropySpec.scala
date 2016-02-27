package org.waman.multiverse.thermal

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EntropySpec
  extends AbstractQuantityAndUnitSpec[EntropyUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[EntropyUnit]

  "3.0 <<entropy unit>> should be converted to the equivalent value in Joule per Kelvin" in {
    __Exercise__
    val conversions =
      Table(
        ("entropies", "expected"),
        (Seq(3.0.J/K, 3.0 J/K, 3.0 (J/K)), 3.0),
        (Seq(3.0.bit, 3.0 bit, 3.0 (bit)), 3.0 * 9.56994016e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Entropy[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut J/K) should equal (%%%%(expected))
      }
    }
  }

  "3.0 J/K should be converted to the equivalent value in other entropy units" in {
    __SetUp__
    val q = 3.0 (J/K)
    __Exercise__
    val conversions =
      Table(
        ("entropies", "expected"),
        (Seq(q.J/K, q J/K, q (J/K)), 3.0),
        (Seq(q.bit, q bit, q (bit)), 3.0 / 9.56994016e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
