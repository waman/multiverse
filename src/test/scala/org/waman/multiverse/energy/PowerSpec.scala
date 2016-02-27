package org.waman.multiverse.energy

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class PowerSpec
  extends AbstractQuantityAndUnitSpec[PowerUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[PowerUnit]

  "3.0 <<power unit>> should be converted to the equivalent value in Watt" in {
    __Exercise__
    val conversions =
      Table(
        ("powers", "expected"),
        (Seq(3.0.W, 3.0 W, 3.0 (W)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Power[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut W) should equal (%%%%(expected))
      }
    }
  }

  "3.0 W should be converted to the equivalent value in other power units" in {
    __SetUp__
    val q = 3.0 (W)
    __Exercise__
    val conversions =
      Table(
        ("powers", "expected"),
        (Seq(q.W, q W, q (W)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
