package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminousIntensitySpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of luminousIntensity" in {
      __SetUp__
      import LuminousIntensityUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[LuminousIntensityUnit])
      __Verify__
      result should contain (Candela)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("luminousIntensity", "expected"),
        (Seq(3.0.cd, 3.0 cd, 3.0 (cd)), 3.0)
      )

    forAll(conversions){ (lis: Seq[LuminousIntensity[Double]], expected: Double) =>
      lis.foreach{ li =>
        (li cd) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 cd

    val conversions =
      Table(
        ("luminousIntensity", "expected"),
        (Seq(value.cd, value cd, value (cd)), 3.0)
      )

    forAll(conversions){ (lis: Seq[Double], expected: Double) =>
      lis.foreach{ li =>
        li should equal (%(expected))
      }
    }
  }
}
