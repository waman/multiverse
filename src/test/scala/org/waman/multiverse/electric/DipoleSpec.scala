package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class DipoleSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of dipole" in {
      __SetUp__
      import DipoleUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[DipoleUnit])
      __Verify__
      result should contain (Debye)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    val conversions =
      Table(
        ("dipole", "expected"),
        (Seq(3.0.C*m, 3.0 C*m, 3.0 (C*m)), 3.0)
      )

    forAll(conversions){ (ds: Seq[Dipole[Double]], expected: Double) =>
      ds.foreach{ d =>
        (d C*m) should equal (%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    val value = 3.0 C*m

    val conversions =
      Table(
        ("dipole", "expected"),
        (Seq(value.C*m, value C*m, value (C*m)), 3.0)
      )

    forAll(conversions){ (ds: Seq[Double], expected: Double) =>
      ds.foreach{ d =>
        d should equal (%(expected))
      }
    }
  }
}
