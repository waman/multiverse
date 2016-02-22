package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class IlluminanceSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of illuminance" in {
      __SetUp__
      import IlluminanceUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[IlluminanceUnit])
      __Verify__
      result should contain (Lux)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("illuminance", "expected"),
        (Seq(3.0.lx, 3.0 lx, 3.0 (lx)), 3.0)
      )

    forAll(conversions){ (is: Seq[Illuminance[Double]], expected: Double) =>
      is.foreach{ i =>
        (i lx) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 lx

    val conversions =
      Table(
        ("illuminance", "expected"),
        (Seq(value.lx, value lx, value (lx)), 3.0)
      )

    forAll(conversions){ (is: Seq[Double], expected: Double) =>
      is.foreach{ i =>
        i should equal (%(expected))
      }
    }
  }
}
