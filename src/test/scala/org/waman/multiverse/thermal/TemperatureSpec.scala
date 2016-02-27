package org.waman.multiverse.thermal

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TemperatureSpec
  extends AbstractQuantityAndUnitSpec[TemperatureUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[TemperatureUnit]

  "3.0 <<temperature unit>> should be converted to the equivalent value in Kelvin" in {
    __Exercise__
    val conversions =
      Table(
        ("temperatures", "expected"),
        (Seq(3.0.K, 3.0 K, 3.0 (K)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Temperature[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut K) should equal (%%%%(expected))
      }
    }
  }

  "3.0 K should be converted to the equivalent value in other temperature units" in {
    __SetUp__
    val q = 3.0 (K)
    __Exercise__
    val conversions =
      Table(
        ("temperatures", "expected"),
        (Seq(q.K, q K, q (K)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
