package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FluxDensitySpec
  extends AbstractQuantityAndUnitSpec[FluxDensityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[FluxDensityUnit]

  "3.0 <<flux density unit>> should be converted to the equivalent value in Tesla" in {
    __Exercise__
    val conversions =
      Table(
        ("flux densities", "expected"),
        (Seq(3.0.T, 3.0 T, 3.0 (T)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[FluxDensity[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut T) should equal (%%%%(expected))
      }
    }
  }

  "3.0 T should be converted to the equivalent value in other flux density units" in {
    __SetUp__
    val q = 3.0 (T)
    __Exercise__
    val conversions =
      Table(
        ("flux densities", "expected"),
        (Seq(q.T, q T, q (T)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
