package org.waman.multiverse.energy

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ActionSpec
  extends AbstractQuantityAndUnitSpec[ActionUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[ActionUnit]

  "3.0 <<action unit>> should be converted to the equivalent value in Joule second" in {
    __Exercise__
    val conversions =
      Table(
        ("actions", "expected"),
        (Seq(3.0.J*s , 3.0 J*s , 3.0 (J*s)) , 3.0),
        (Seq(3.0.hbar, 3.0 hbar, 3.0 (hbar)), 3.0 * 1.05457168e-34),
        (Seq(3.0.ħ   , 3.0 ħ   , 3.0 (ħ))   , 3.0 * 1.05457168e-34)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Action[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut J*s) should equal (%%%%(expected))
      }
    }
  }

  "3.0 J*s should be converted to the equivalent value in other action units" in {
    __SetUp__
    val q = 3.0 (J*s)
    __Exercise__
    val conversions =
      Table(
        ("actions", "expected"),
        (Seq(q.J*s , q J*s , q (J*s)) , 3.0),
        (Seq(q.hbar, q hbar, q (hbar)), 3.0 / 1.05457168e-34),
        (Seq(q.ħ   , q ħ   , q (ħ))   , 3.0 / 1.05457168e-34)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
