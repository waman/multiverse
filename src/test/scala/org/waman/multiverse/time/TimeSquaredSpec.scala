package org.waman.multiverse.time

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TimeSquaredSpec
  extends AbstractQuantityAndUnitSpec[TimeSquaredUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[TimeSquaredUnit]

  "3.0 <<time squared unit>> should be converted to the equivalent value in second squared" in {
    __Exercise__
    val conversions =
      Table(
        ("seq of time squared", "expected"),
        (Seq(3.0.s2, 3.0 s2, 3.0 (s2)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[TimeSquared[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut s2) should equal (%%%%(expected))
      }
    }
  }

  "3.0 s2 should be converted to the equivalent value in other time squared units" in {
    __SetUp__
    val q = 3.0 (s2)
    __Exercise__
    val conversions =
      Table(
        ("seq of time squared", "expected"),
        (Seq(q.s2, q s2, q (s2)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
