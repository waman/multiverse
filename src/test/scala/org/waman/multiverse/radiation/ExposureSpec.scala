package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ExposureSpec
  extends AbstractQuantityAndUnitSpec[ExposureUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[ExposureUnit]

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    __Exercise__
    val conversions =
      Table(
        ("exposures", "expected"),
        (Seq(3.0.C/kg, 3.0 C/kg, 3.0 (C/kg)), 3.0),
        (Seq(3.0.R   , 3.0 R   , 3.0 (R))   , 3.0 * 2.58e-4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Exposure[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut C/kg) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (C/kg)
    __Exercise__
    val conversions =
      Table(
        ("exposures", "expected"),
        (Seq(value.C/kg, value C/kg, value (C/kg)), 3.0),
        (Seq(value.R   , value R   , value (R))   , 3.0 / 2.58e-4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
