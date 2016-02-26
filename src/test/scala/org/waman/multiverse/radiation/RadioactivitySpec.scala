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

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
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

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (Bq)
    __Exercise__
    val conversions =
      Table(
        ("radioactivities", "expected"),
        (Seq(value.Bq, value Bq, value (Bq)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
