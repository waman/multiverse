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

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
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

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (T)
    __Exercise__
    val conversions =
      Table(
        ("flux densities", "expected"),
        (Seq(value.T, value T, value (T)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
