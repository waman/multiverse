package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ResistanceSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of resistance" in {
      __SetUp__
      import ResistanceUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[ResistanceUnit])
      __Verify__
      result should contain (Ohm)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("resistance", "expected"),
        (Seq(3.0.Ω, 3.0 Ω, 3.0 (Ω)), 3.0)
      )

    forAll(conversions){ (rs: Seq[Resistance[Double]], expected: Double) =>
      rs.foreach{ r =>
        (r Ω) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 Ω

    val conversions =
      Table(
        ("resistance", "expected"),
        (Seq(value.Ω, value Ω, value (Ω)), 3.0)
      )

    forAll(conversions){ (rs: Seq[Double], expected: Double) =>
      rs.foreach{ r =>
        r should equal (%(expected))
      }
    }
  }
}
