package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EquivalentDoseSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of equivalentDose" in {
      __SetUp__
      import EquivalentDoseUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[EquivalentDoseUnit])
      __Verify__
      result should contain (Sievert)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("equivalentDose", "expected"),
        (Seq(3.0.Sv, 3.0 Sv, 3.0 (Sv)), 3.0)
      )

    forAll(conversions){ (eds: Seq[EquivalentDose[Double]], expected: Double) =>
      eds.foreach{ ed =>
        (ed Sv) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 Sv

    val conversions =
      Table(
        ("equivalentDose", "expected"),
        (Seq(value.Sv, value Sv, value (Sv)), 3.0)
      )

    forAll(conversions){ (eds: Seq[Double], expected: Double) =>
      eds.foreach{ ed =>
        ed should equal (%(expected))
      }
    }
  }
}
