package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminousFluxSpec
  extends AbstractQuantityAndUnitSpec[LuminousFluxUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[LuminousFluxUnit]

  "3.0 <<luminous flux unit>> should be converted to the equivalent value in lumen" in {
    __Exercise__
    val conversions =
      Table(
        ("luminous fluxes", "expected"),
        (Seq(3.0.lm, 3.0 lm, 3.0 (lm)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[LuminousFlux[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut lm) should equal (%%%%(expected))
      }
    }
  }

  "3.0 lm should be converted to the equivalent value in other luminus flux units" in {
    __SetUp__
    val q = 3.0 (lm)
    __Exercise__
    val conversions =
      Table(
        ("luminous fluxes", "expected"),
        (Seq(q.lm, q lm, q (lm)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
