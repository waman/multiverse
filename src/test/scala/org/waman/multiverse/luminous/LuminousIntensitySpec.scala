package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminousIntensitySpec
  extends AbstractQuantityAndUnitSpec[LuminousIntensityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[LuminousIntensityUnit]

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    __Exercise__
    val conversions =
      Table(
        ("luminous intensities", "expected"),
        (Seq(3.0.cd, 3.0 cd, 3.0 (cd)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[LuminousIntensity[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut cd) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (cd)
    __Exercise__
    val conversions =
      Table(
        ("luminous intensities", "expected"),
        (Seq(value.cd, value cd, value (cd)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
