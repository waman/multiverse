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
    __Exercise__
    val conversions =
      Table(
        ("forces", "expected"),
        (Seq(3.0.N, 3.0 N, 3.0 (N)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Force[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut N) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting second unit to other units like 1.0 s => 1000.0 ms" in {
    __SetUp__
    val value = 3.0 (N)
    __Exercise__
    val conversions =
      Table(
        ("forces", "expected"),
        (Seq(value.N, value N, value (N)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
