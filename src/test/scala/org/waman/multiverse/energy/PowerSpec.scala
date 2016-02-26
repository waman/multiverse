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

  "Tests where converting from some units to kg like 3.0 kW => 3e3 W" in {
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

  "Tests where converting a power unit to other units like 3.0 W => 3e-3 kW" in {
    __SetUp__
    val value = 3.0 (W)
    __Exercise__
    val conversions =
      Table(
        ("powers", "expected"),
        (Seq(value.W, value W, value (W)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
