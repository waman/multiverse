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

  "Tests where converting from some units to kg like 3.0 kJ => 3e3 J" in {
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

  "Tests where converting a action unit to other units like 3.0 J => 3e-3 kJ" in {
    __SetUp__
    val value = 3.0 (J*s)
    __Exercise__
    val conversions =
      Table(
        ("actions", "expected"),
        (Seq(value.J*s , value J*s , value (J*s)) , 3.0),
        (Seq(value.hbar, value hbar, value (hbar)), 3.0 / 1.05457168e-34),
        (Seq(value.ħ   , value ħ   , value (ħ))   , 3.0 / 1.05457168e-34)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
