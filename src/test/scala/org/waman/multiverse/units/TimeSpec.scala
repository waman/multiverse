package org.waman.multiverse.units

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.TimeUnits._

class TimeSpec extends MultiverseCustomSpec {

  "3.0 <<length unit>> should be converted to the equivalent value in metre" in {
    // Exercise
    val conversions =
      Table(
        ("length", "expected"),
        (3.0(ms), 3e-3),
        (3.0(s) , 3.0),
        (3.0(min), 3.0*60.0),
        (3.0(h), 3.0*3600.0)
      )
    // Verify
    forAll(conversions){ (sut: Length[Double], expected: Double) =>
      sut(s) should equal (%%%%(expected))
    }
  }

  "3.0(m) should be converted to the equivalent value in other length units" in {
    // SetUp
    val q = 3.0 (s)
    // Exercise
    val conversions =
      Table(
        ("length", "expected"),
        (q(ms), 3e3),
        (q(s) , 3.0),
        (q(min), 3.0/60.0),
        (q(h), 3.0/3600.0)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }
}
