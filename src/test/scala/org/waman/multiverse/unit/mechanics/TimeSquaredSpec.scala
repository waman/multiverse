package org.waman.multiverse.unit.mechanics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.basic.TimeUnits._
import org.waman.multiverse.unit.mechanics.TimeSquaredUnits._

class TimeSquaredSpec extends MultiverseCustomSpec {

  "Unit" - {

    "All of s2, s*s, and s.square should return equivalent objects" in {
      s2 should equal (s*s)
      s2 should equal (s.square)

      ms*ms should equal (ms.square)
    }

    "The name property of TimeSquared unit object should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("time squared unit", "expected"),
          (s2 , "second squared"),
          (ms*ms, "millisecond squared"),
          (s*ms, "second times millisecond")
        )
      // Verify
      forAll(conversions){ (sut: TimeSquaredUnit, expected: String) =>
        sut.name should equal (expected)
      }
    }

    "The symbol property of TimeSquared unit object should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("time squared unit", "expected"),
          (s2 , "s²"),
          (ms*ms, "ms²"),
          (s*ms, "s*ms")
        )
      // Verify
      forAll(conversions){ (sut: TimeSquaredUnit, expected: String) =>
        sut.symbol should equal (expected)
      }
    }

    "The aliases property of TimeSquared unit object should return the proper Seq of String" in {
      // Exercise
      val conversions =
        Table(
          ("time squared unit", "expected"),
          (s2 , Seq("s2", "sec²", "sec2", "s.square", "sec.square", "s*s", "sec*sec")),
          (ms*ms, Seq("ms.square", "msec.square", "ms*ms", "msec*msec")),
          (s*ms, Seq("s*msec", "sec*ms", "sec*msec"))
        )
      // Verify
      forAll(conversions){ (sut: TimeSquaredUnit, expected: Seq[String]) =>
        sut.aliases should contain theSameElementsAs expected
      }
    }
  }

  "Quantity" - {

    "3.0 <<time squared unit>> should be converted to the equivalent value in second squared" in {
      // Exercise
      val conversions =
        Table(
          ("time squared", "expected"),
          (3.0(s2) , 3.0),
          (3.0(s*s), 3.0),
          (3.0(s.square), 3.0),
          (3.0(ms*ms), 3.0*0.000001),
          (3.0(ms.square), 3.0*0.000001),
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

    "The square method of Time should return the proper TimeSquared object" in {
      // SetUp
      val expected = 9(ms2)
      val time = 3(ms)
      // Exercise
      val sut = time.square
      // Verify
      sut should equal (expected)
    }
  }

  "[SOURCE GENERATION]" - {

    "second_squared should have the proper symbol and some aliases" in {
      // SetUp
      val expected = s.square
      // Exercise
      val conversions =
        Table(
          "time squared unit",
          TimeSquaredUnitObjects.second_squared,
          s2,
          `s²`,
          sec2,
          `sec²`
        )
      // Verify
      forAll(conversions){ sut: TimeSquaredUnit =>
        sut should equal (expected)
      }
    }

    "microsecond_squared should have the proper symbol and some aliases" in {
      // SetUp
      val expected = mcs.square
      // Exercise
      val conversions =
        Table(
          "time squared unit",
          TimeSquaredUnitObjects.microsecond_squared,
          μs2,
          mcs2,
          `μs²`,
          `mcs²`,
          μsec2,
          mcsec2,
          `μsec²`,
          `mcsec²`
        )
      // Verify
      forAll(conversions){ sut: TimeSquaredUnit =>
        sut should equal (expected)
      }
    }
  }
}
