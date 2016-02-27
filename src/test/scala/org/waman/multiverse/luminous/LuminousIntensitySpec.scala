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

  "3.0 <<luminous intensity unit>> should be converted to the equivalent value in candera" in {
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

  "3.0 cd should be converted to the equivalent value in other luminous intensity units" in {
    __SetUp__
    val q = 3.0 (cd)
    __Exercise__
    val conversions =
      Table(
        ("luminous intensities", "expected"),
        (Seq(q.cd, q cd, q (cd)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
