package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class RadioactivitySpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of radioactivity" in {
      __SetUp__
      import RadioactivityUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[RadioactivityUnit])
      __Verify__
      result should contain (Becquerel)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("radioactivity", "expected"),
        (Seq(3.0.Bq, 3.0 Bq, 3.0 (Bq)), 3.0)
      )

    forAll(conversions){ (ras: Seq[Radioactivity[Double]], expected: Double) =>
      ras.foreach{ ra =>
        (ra Bq) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 Bq

    val conversions =
      Table(
        ("radioactivity", "expected"),
        (Seq(value.Bq, value Bq, value (Bq)), 3.0)
      )

    forAll(conversions){ (ras: Seq[Double], expected: Double) =>
      ras.foreach{ ra =>
        ra should equal (%(expected))
      }
    }
  }
}
