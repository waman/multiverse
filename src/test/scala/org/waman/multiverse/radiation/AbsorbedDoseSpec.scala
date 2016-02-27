package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AbsorbedDoseSpec
  extends AbstractQuantityAndUnitSpec[AbsorbedDoseUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[AbsorbedDoseUnit]

  "3.0 <<absorbed dose unit>> should be converted to the equivalent value in Gray" in {
    __Exercise__
    val conversions =
      Table(
        ("absorbedDoses", "expected"),
        (Seq(3.0.Gy, 3.0 Gy, 3.0 (Gy)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[AbsorbedDose[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Gy) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Gy should be converted to the equivalent value in other absorbed dose units" in {
    __SetUp__
    val q = 3.0 (Gy)
    __Exercise__
    val conversions =
      Table(
        ("absorbedDoses", "expected"),
        (Seq(q.Gy, q Gy, q (Gy)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
