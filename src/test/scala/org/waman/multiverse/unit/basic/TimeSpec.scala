package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.basic.TimeAttributes.gregorian
import org.waman.multiverse.unit.basic.TimeUnits._
import org.waman.multiverse.implicits._

class TimeSpec extends MultiverseCustomSpec {

  "3.0 <<time unit>> should be converted to the equivalent value in second" in {
    // Exercise
    val conversions =
      Table(
        ("time", "expected"),
        (3.0(ms), 3e-3),
        (3.0(s) , 3.0),
        (3.0(min), 3.0*60.0),
        (3.0(h), 3.0*3600.0)
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
        (q(h), 3.0/3600.0)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }

  "[SOURCE GENERATION]" - {

    "The default attribute should provide the value of interval and baseUnit for the parent unit" in {
      // Exercise
      val conversions =
        Table(
          ("parent", "expected"),
          (mo, mo(gregorian)),
          (yr, yr(gregorian)),
          (dec, dec(gregorian)),
          (century, century(gregorian))
        )
      // Verify
      forAll(conversions){ (sut: TimeUnit, expected: TimeUnit) =>
        sut.interval should equal (expected.interval)
      }
    }

    "prefixed unit 'microsecond' should have combinated aliases of a base unit and prefix" in {
      // SetUp
      val expected = Seq("μsec", "mcs", "mcsec")
      // Exercise
      val sut = TimeUnitObjects.microsecond
      // Verify
      sut.aliases should contain theSameElementsAs expected
      sut.symbol should be ("μs")
    }
  }
}
