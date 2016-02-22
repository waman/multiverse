package org.waman.multiverse.thermal

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EntropySpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of entropy" in {
      __SetUp__
      import EntropyUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[EntropyUnit])
      __Verify__
      result should contain (Bit)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("entropy", "expected"),
        (Seq(3.0.J/K, 3.0 J/K, 3.0 (J/K)), 3.0),
        (Seq(3.0.bit, 3.0 bit, 3.0 (bit)), 3.0 * 9.56994016e-24)
      )

    forAll(conversions){ (es: Seq[Entropy[Double]], expected: Double) =>
      es.foreach{ e =>
        (e J/K) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 J/K

    val conversions =
      Table(
        ("entropy", "expected"),
        (Seq(value.J/K, value J/K, value (J/K)), 3.0),
        (Seq(value.bit, value bit, value (bit)), 3.0 / 9.56994016e-24)
      )

    forAll(conversions){ (es: Seq[Double], expected: Double) =>
      es.foreach{ e =>
        e should equal (%(expected))
      }
    }
  }
}
