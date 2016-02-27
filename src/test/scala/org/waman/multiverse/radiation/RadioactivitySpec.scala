package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class RadioactivitySpec
  extends AbstractQuantityAndUnitSpec[RadioactivityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[RadioactivityUnit]

  "3.0 <<radioactivity unit>> should be converted to the equivalent value in Becquerel" in {
    __Exercise__
    val conversions =
      Table(
        ("radioactivities", "expected"),
        (Seq(3.0.Bq, 3.0 Bq, 3.0 (Bq)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Radioactivity[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Bq) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Bq should be converted to the equivalent value in other radioactivity units" in {
    __SetUp__
    val q = 3.0 (Bq)
    __Exercise__
    val conversions =
      Table(
        ("radioactivities", "expected"),
        (Seq(q.Bq, q Bq, q (Bq)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
