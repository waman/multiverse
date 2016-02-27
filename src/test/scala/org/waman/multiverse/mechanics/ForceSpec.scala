package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ForceSpec
  extends AbstractQuantityAndUnitSpec[ForceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[ForceUnit]

  "3.0 <<force unit>> should be converted to the equivalent value in Newton" in {
    __Exercise__
    val conversions =
      Table(
        ("forces", "expected"),
        (Seq(3.0.N, 3.0 N, 3.0 (N)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Force[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut N) should equal (%%%%(expected))
      }
    }
  }

  "3.0 N should be converted to the equivalent value in other force units" in {
    __SetUp__
    val q = 3.0 (N)
    __Exercise__
    val conversions =
      Table(
        ("forces", "expected"),
        (Seq(q.N, q N, q (N)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
