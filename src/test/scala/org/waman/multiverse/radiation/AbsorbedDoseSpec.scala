package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AbsorbedDoseSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of absorbedDose" in {
      __SetUp__
      import AbsorbedDoseUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[AbsorbedDoseUnit])
      __Verify__
      result should contain (Gray)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("absorbedDose", "expected"),
        (Seq(3.0.Gy, 3.0 Gy, 3.0 (Gy)), 3.0)
      )

    forAll(conversions){ (ads: Seq[AbsorbedDose[Double]], expected: Double) =>
      ads.foreach{ ad =>
        (ad Gy) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 Gy

    val conversions =
      Table(
        ("absorbedDose", "expected"),
        (Seq(value.Gy, value Gy, value (Gy)), 3.0)
      )

    forAll(conversions){ (ads: Seq[Double], expected: Double) =>
      ads.foreach{ ad =>
        ad should equal (%(expected))
      }
    }
  }
}
