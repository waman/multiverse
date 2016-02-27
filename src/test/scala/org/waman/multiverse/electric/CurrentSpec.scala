package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class CurrentSpec
  extends AbstractQuantityAndUnitSpec[CurrentUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[CurrentUnit]

  "3.0 <<current unit>> should be converted to the equivalent value in Ampere" in {
    __Exercise__
    val conversions =
      Table(
        ("currents", "expected"),
        (Seq(3.0.A, 3.0 A, 3.0 (A)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Current[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut A) should equal (%%%%(expected))
      }
    }
  }

  "3.0 m should be converted to the equivalent value in other current units" in {
    __SetUp__
    val q = 3.0 (A)
    __Exercise__
    val conversions =
      Table(
        ("currents", "expected"),
        (Seq(q.A, q A, q (A)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
