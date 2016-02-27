package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ChargeSpec
  extends AbstractQuantityAndUnitSpec[ChargeUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[ChargeUnit]

  "3.0 <<charge unit>> should be converted to the equivalent value in Coulomb" in {
    __Exercise__
    val conversions =
      Table(
        ("charges", "expected"),
        (Seq(3.0.C, 3.0 C, 3.0 (C)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Charge[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut C) should equal (%%%%(expected))
      }
    }
  }

  "3.0 m should be converted to the equivalent value in other charge units" in {
    __SetUp__
    val q = 3.0 (C)
    __Exercise__
    val conversions =
      Table(
        ("charges", "expected"),
        (Seq(q.C, q C, q (C)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
