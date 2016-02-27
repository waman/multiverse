package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class InductanceSpec
  extends AbstractQuantityAndUnitSpec[InductanceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[InductanceUnit]

  "3.0 <<inductance unit>> should be converted to the equivalent value in Henry" in {
    __Exercise__
    val conversions =
      Table(
        ("inductances", "expected"),
        (Seq(3.0.H, 3.0 H, 3.0 (H)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Inductance[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut H) should equal (%%%%(expected))
      }
    }
  }

  "3.0 H should be converted to the equivalent value in other inductance units" in {
    __SetUp__
    val q = 3.0 (H)
    __Exercise__
    val conversions =
      Table(
        ("inductances", "expected"),
        (Seq(q.H, q H, q (H)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
