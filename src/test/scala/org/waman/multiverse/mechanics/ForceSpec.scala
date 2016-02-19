package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ForceSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of force" in {
    __SetUp__
    import ForceUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[ForceUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      Newton,
      KiloGramForce,
      Dyne
    )
  }

  "Tests where converting from some units to second like 1.0 ms => 0.001 s" in {
    val conversions =
      Table(
        ("force", "expected"),
        (Seq(3.0.N, 3.0 N, 3.0 (N)), 3.0)
      )

    forAll(conversions){ (ts: Seq[Force[Double]], expected: Double) =>
      ts.foreach{ t =>
        (t N) should equal (%(expected))
      }
    }
  }

  "Tests where converting second unit to other units like 1.0 s => 1000.0 ms" in {
    val value = 3.0 N

    val conversions =
      Table(
        ("force", "expected"),
        (Seq(value.N, value N, value (N)), 3.0)
      )

    forAll(conversions){ (fs: Seq[Double], expected: Double) =>
      fs.foreach{ f =>
        f should equal (%(expected))
      }
    }
  }
}
