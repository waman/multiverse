package org.waman.multiverse.energy

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ActionSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of action" in {
    __SetUp__
    import ActionUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[ActionUnit])
    __Verify__
    result should contain (AtomicUnitOfAction)
  }

  "Tests where converting from some units to kg like 3.0 kJ => 3e3 J" in {
    val conversions =
      Table(
        ("action", "expected"),
        (Seq(3.0.J*s , 3.0 J*s , 3.0 (J*s)) , 3.0),
        (Seq(3.0.hbar, 3.0 hbar, 3.0 (hbar)), 3.0 * 1.05457168e-34),
        (Seq(3.0.ħ   , 3.0 ħ   , 3.0 (ħ))   , 3.0 * 1.05457168e-34)
      )

    forAll(conversions){ (as: Seq[Action[Double]], expected: Double) =>
      as.foreach{ a =>
        (a J*s) should equal (%(expected))
      }
    }
  }

  "Tests where converting a action unit to other units like 3.0 J => 3e-3 kJ" in {
    import PredefinedActionUnit.hbar
    val value = 3.0 J*s

    val conversions =
      Table(
        ("action", "expected"),
        (Seq(value.J*s , value J*s , value (J*s)) , 3.0),
        (Seq(value.hbar, value hbar, value (hbar)), 3.0 / 1.05457168e-34),
        (Seq(value.ħ   , value ħ   , value (ħ))   , 3.0 / 1.05457168e-34)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
