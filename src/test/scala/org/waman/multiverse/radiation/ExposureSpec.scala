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

  "3.0 <<exposure unit>> should be converted to the equivalent value in Coulomb per kilogram" in {
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

  "3.0 C/kg should be converted to the equivalent value in other exposure units" in {
    __SetUp__
    val q = 3.0 (C/kg)
    __Exercise__
    val conversions =
      Table(
        ("exposures", "expected"),
        (Seq(q.C/kg, q C/kg, q (C/kg)), 3.0),
        (Seq(q.R   , q R   , q (R))   , 3.0 / 2.58e-4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
