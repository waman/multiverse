package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class CurrentSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of current" in {
      __SetUp__
      import CurrentUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[CurrentUnit])
      __Verify__
      result should contain (Ampere)
    }

  "Tests where converting from some units to A like 3.0 mA => 3e-3 A" in {
    val conversions =
      Table(
        ("current", "expected"),
        (Seq(3.0.A, 3.0 A, 3.0 (A)), 3.0)
      )

    forAll(conversions){ (cs: Seq[Current[Double]], expected: Double) =>
      cs.foreach{ c =>
        (c A) should equal (%(expected))
      }
    }
  }

  "Tests where converting an Ampere to other units like 3.0 A => 3e3 mA" in {
    val value = 3.0 A

    val conversions =
      Table(
        ("current", "expected"),
        (Seq(value.A, value A, value (A)), 3.0)
      )

    forAll(conversions){ (cs: Seq[Double], expected: Double) =>
      cs.foreach{ c =>
        c should equal (%(expected))
      }
    }
  }
}
