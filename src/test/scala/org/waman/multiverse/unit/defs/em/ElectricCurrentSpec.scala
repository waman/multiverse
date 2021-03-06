package org.waman.multiverse.unit.defs.em

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.custom.BasicUnits._
import org.waman.multiverse.unit.defs.em.ElectricCurrentUnits._
import org.waman.multiverse.unit.defs.em.ElectricChargeUnits._

class ElectricCurrentSpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<current unit>> should be converted to the equivalent value in ampare" in {
      // Exercise
      val conversions =
        Table(
          ("current", "expected"),
          (3.0(A), 3.0),
          (3.0(abamp), 3.0 * 10.0),
          (3.0(esu/s), 3.0 * 3.335641e-10)
        )
      // Verify
      forAll(conversions) { (sut: ElectricCurrent[Double], expected: Double) =>
        sut(A) should equal(%%%(expected))
      }
    }

    "3.0(A) should be converted to the equivalent value in other current units" in {
      // SetUp
      val q = 3.0(A)
      // Exercise
      val conversions =
        Table(
          ("current", "expected"),
          (q(A), 3.0),
          (q(abamp), 0.3),
          (q(esu/s), 3.0 / 3.335641e-10)
        )
      // Verify
      forAll(conversions) { (sut: Double, expected: Double) =>
        sut should equal(%%%(expected))
      }
    }
  }
}
