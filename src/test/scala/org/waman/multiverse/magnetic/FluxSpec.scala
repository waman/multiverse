package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FluxSpec
  extends AbstractQuantityAndUnitSpec[FluxUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[FluxUnit]

  "3.0 <<flux unit>> should be converted to the equivalent value in Weber" in {
    __Exercise__
    val conversions =
      Table(
        ("fluxes", "expected"),
        (Seq(3.0.Wb, 3.0 Wb, 3.0 (Wb)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Flux[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Wb) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Wb should be converted to the equivalent value in other flux units" in {
    __SetUp__
    val q = 3.0 (Wb)
    __Exercise__
    val conversions =
      Table(
        ("fluxes", "expected"),
        (Seq(q.Wb, q Wb, q (Wb)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
