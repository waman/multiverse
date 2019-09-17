package org.waman.multiverse.unit.mechanics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.MechanicsUnits._
import org.waman.multiverse.unit.basic.TimeUnits.{ms, s}

class TimeSquaredSpec extends MultiverseCustomSpec {

  "symbol property of TimeSquared should return the proper string" in {
    // Exercise
    val conversions =
      Table(
        ("time squared unit", "expected"),
        (s2 , "s²"),
        (s*s, "s²"),
        (ms*ms, "ms²"),
        (s*ms, "s*ms")
      )
    // Verify
    forAll(conversions){ (sut: TimeSquaredUnit, expected: String) =>
      sut.symbol should equal (expected)
    }
  }

  "3.0 <<time squared unit>> should be converted to the equivalent value in second squared" in {
    // Exercise
    val conversions =
      Table(
        ("time squared", "expected"),
        (3.0(s2) , 3.0),
        (3.0(s*s), 3.0),
        (3.0(ms*ms), 3.0*0.000001),
        (3.0(s*ms), 3.0*0.001)
      )
    // Verify
    forAll(conversions){ (sut: TimeSquared[Double], expected: Double) =>
      sut(s2) should equal (%%%%(expected))
    }
  }

  "3.0(s2) should be converted to the equivalent value in other time squared units" in {
    // SetUp
    val q = 3.0 (s2)
    // Exercise
    val conversions =
      Table(
        ("time squared", "expected"),
        (q(s2) , 3.0),
        (q(s*s), 3.0),
        (q(ms*ms), 3.0*1000000.0),
        (q(s*ms), 3.0*1000.0)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }
}
