package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EquivalentDoseRateSpec extends MultiverseCustomSpec with PropertyChecks{


  "Quotient equivalent dose rate unit" - {

    "Equivalent dose rate unit of kSv/h should equal 1.0 / 3600.0 Sv/s" in {
      __Exercise__
      val sut = kSv/h
      __Verify__
      sut.unitInSievertPerSecond.toDouble should equal (%%%%(1000.0 / 3600.0))
    }

    "3.0 kSv/h should equal 3.0 * 1000.0 / 3600.0 Sv/s" in {
      __Exercise__
      val conversions =
        Table(
          ("equivalent dose rate", "expected"),
          (3.0.kSv/h, 3.0 * 1000.0 / 3600.0),
          (3.0 kSv/h, 3.0 * 1000.0 / 3600.0),
          (3.0 (kSv/h), 3.0 * 1000.0 / 3600.0)
        )
      __Verify__
      forAll(conversions){ (sut: EquivalentDoseRate[Double], expected: Double) =>
        sut.Sv/s should equal (%%%%(expected))
      }
    }

    "3.0 Sv/s should equal 3.0 * 3600.0 / 1000.0 kSv/h" in {
      __SetUp__
      val q = 3.0 (Sv/s)
      val expected = 3.0 * 3600.0 / 1000.0
      __Exercise__
      val conversions =
        Table(
          ("equivalent dose rate", "expected"),
          (q.kSv/h, expected),
          (q kSv/h, expected),
          (q (kSv/h), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
