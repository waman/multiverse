package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ChargeSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of charge" in {
      __SetUp__
      import ChargeUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[ChargeUnit])
      __Verify__
      result should contain (Coulomb)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("charge", "expected"),
        (Seq(3.0.C, 3.0 C, 3.0 (C)), 3.0)
      )

    forAll(conversions){ (cs: Seq[Charge[Double]], expected: Double) =>
      cs.foreach{ c =>
        (c C) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 C

    val conversions =
      Table(
        ("charge", "expected"),
        (Seq(value.C, value C, value (C)), 3.0)
      )

    forAll(conversions){ (cs: Seq[Double], expected: Double) =>
      cs.foreach{ c =>
        c should equal (%(expected))
      }
    }
  }
}
