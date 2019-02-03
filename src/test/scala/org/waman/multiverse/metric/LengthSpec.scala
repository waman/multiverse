package org.waman.multiverse.metric

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.LengthUnits._

class LengthSpec extends MultiverseCustomSpec {

  "3.0 <<length unit>> should be converted to the equivalent value in metre" in {
    // Exercise
    val conversions =
      Table(
        ("length", "expected"),
        (3.0(mm), 3e-3),
        (3.0(m) , 3.0),
        (3.0(km), 3e3)
      )
    // Verify
    forAll(conversions){ (sut: Length[Double], expected: Double) =>
      sut(m) should equal (%%%%(expected))
    }
  }

  "3.0(m) should be converted to the equivalent value in other length units" in {
    // SetUp
    val q: Length[Double] = 3.0 (m)
    // Exercise
    val conversions =
      Table(
        ("length", "expected"),
        (q(mm), 3e3),
        (q(m) , 3.0),
        (q(km), 3e-3)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }
}
