package org.waman.multiverse.unit.theramal

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.Constants
import org.waman.multiverse.unit.thermal.EntropyUnit
import org.waman.multiverse.unit.thermal.EntropyUnitObjects._
import spire.implicits._
import spire.math.Real

class EntropySpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION]" - {

    "nat, bit, and ban should have the proper interval values" in {
      // Exercise
      val conversions =
        Table(
          ("entropy unit", "expected"),
          (nat, Constants.BoltzmannConstant),
          (bit, Real(2).log() * Constants.BoltzmannConstant),
          (ban, Real(10).log() * Constants.BoltzmannConstant)
        )
      // Verify
      forAll(conversions){ (sut: EntropyUnit, expected: Real) =>
        sut.interval should equal (expected)
      }
    }

    "millibyte unit should be defined (excludePrefixes should work well)" in {
      "kilobyte" should compile
      "millibyte" shouldNot compile
    }
  }
}
