package org.waman.multiverse.thermal

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EntropySpec
  extends AbstractQuantityAndUnitSpec[EntropyUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[EntropyUnit]

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    __Exercise__
    val conversions =
      Table(
        ("entropies", "expected"),
        (Seq(3.0.J/K, 3.0 J/K, 3.0 (J/K)), 3.0),
        (Seq(3.0.bit, 3.0 bit, 3.0 (bit)), 3.0 * 9.56994016e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Entropy[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut J/K) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (J/K)
    __Exercise__
    val conversions =
      Table(
        ("entropies", "expected"),
        (Seq(value.J/K, value J/K, value (J/K)), 3.0),
        (Seq(value.bit, value bit, value (bit)), 3.0 / 9.56994016e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
