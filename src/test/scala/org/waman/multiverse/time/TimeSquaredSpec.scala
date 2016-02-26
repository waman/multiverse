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

  "Tests where converting from some units to kg like 3.0 g => 3e-3 kg" in {
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

  "Tests where converting a time squared unit to other units like 3.0 kg/m3 => 3e-3 g/cm3" in {
    __SetUp__
    val value = 3.0 (s2)
    __Exercise__
    val conversions =
      Table(
        ("seq of time squared", "expected"),
        (Seq(value.s2, value s2, value (s2)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
