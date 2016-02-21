package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FluxDensitySpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of flux density" in {
      __SetUp__
      import FluxDensityUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[FluxDensityUnit])
      __Verify__
      result should contain (Tesla)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("fluxDensity", "expected"),
        (Seq(3.0.T, 3.0 T, 3.0 (T)), 3.0)
      )

    forAll(conversions){ (fds: Seq[FluxDensity[Double]], expected: Double) =>
      fds.foreach{ fd =>
        (fd T) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 T

    val conversions =
      Table(
        ("fluxDensity", "expected"),
        (Seq(value.T, value T, value (T)), 3.0)
      )

    forAll(conversions){ (fds: Seq[Double], expected: Double) =>
      fds.foreach{ fd =>
        fd should equal (%(expected))
      }
    }
  }
}
