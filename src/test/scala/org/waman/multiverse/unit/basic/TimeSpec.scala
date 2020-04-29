package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._

class TimeSpec extends MultiverseCustomSpec {

  "Quantity" - {
    import org.waman.multiverse.unit.basic.TimeUnits._

    "3.0 <<time unit>> should be converted to the equivalent value in second" in {
      // Exercise
      val conversions =
        Table(
          ("time", "expected"),
          (3.0(ms), 3e-3),
          (3.0(s) , 3.0),
          (3.0(min), 3.0*60.0),
          (3.0(h), 3.0*3600.0),

          (3.0(yr), 3.0*365.2425*24*3600),
          (3.0(yr(gregorian)), 3.0*365.2425*24*3600),
          (3.0(yr(julian)), 3.0*365.25*24*3600)
        )
      // Verify
      forAll(conversions){ (sut: Time[Double], expected: Double) =>
        sut(s) should equal (%%%%(expected))
      }
    }

    "3.0(s) should be converted to the equivalent value in other time units" in {
      // SetUp
      val q = 3.0 (s)
      // Exercise
      val conversions =
        Table(
          ("time", "expected"),
          (q(ms), 3e3),
          (q(s) , 3.0),
          (q(min), 3.0/60.0),
          (q(h), 3.0/3600.0),

          (q(yr), 3.0/(365.2425*24*3600)),
          (q(yr(gregorian)), 3.0/(365.2425*24*3600)),
          (q(yr(julian)), 3.0/(365.25*24*3600))
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
