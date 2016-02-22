package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminousFluxSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of luminousFlux" in {
      __SetUp__
      import LuminousFluxUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[LuminousFluxUnit])
      __Verify__
      result should contain (Lumen)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("luminousFlux", "expected"),
        (Seq(3.0.lm, 3.0 lm, 3.0 (lm)), 3.0)
      )

    forAll(conversions){ (lfs: Seq[LuminousFlux[Double]], expected: Double) =>
      lfs.foreach{ lf =>
        (lf lm) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 lm

    val conversions =
      Table(
        ("luminousFlux", "expected"),
        (Seq(value.lm, value lm, value (lm)), 3.0)
      )

    forAll(conversions){ (lfs: Seq[Double], expected: Double) =>
      lfs.foreach{ lf =>
        lf should equal (%(expected))
      }
    }
  }
}
