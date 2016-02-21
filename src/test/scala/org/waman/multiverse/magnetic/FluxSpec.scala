package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FluxSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of flux" in {
      __SetUp__
      import FluxUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[FluxUnit])
      __Verify__
      result should contain (Weber)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("flux", "expected"),
        (Seq(3.0.Wb, 3.0 Wb, 3.0 (Wb)), 3.0)
      )

    forAll(conversions){ (fs: Seq[Flux[Double]], expected: Double) =>
      fs.foreach{ f =>
        (f Wb) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 Wb

    val conversions =
      Table(
        ("flux", "expected"),
        (Seq(value.Wb, value Wb, value (Wb)), 3.0)
      )

    forAll(conversions){ (fs: Seq[Double], expected: Double) =>
      fs.foreach{ f =>
        f should equal (%(expected))
      }
    }
  }
}
