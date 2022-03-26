package org.waman.multiverse.unit.defs.chm

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.defs.chm.CatalysisUnits._

class CatalysisSpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<catalysis unit>> should be converted to the equivalent value in katal" in {
      // Exercise
      val conversions =
        Table(
          ("catalysis", "expected"),
          (3.0(kat) , 3.0),
          (3.0(nkat), 3e-9),
          (3.0(U), 3e-6/60.0)
        )
      // Verify
      forAll(conversions){ (sut: Catalysis[Double], expected: Double) =>
        sut(kat) should equal (%%%%(expected))
      }
    }

    "3.0(kg) should be converted to the equivalent value in other catalysis units" in {
      // SetUp
      val q = 3.0 (kat)
      // Exercise
      val conversions =
        Table(
          ("catalysis", "expected"),
          (q(kat) , 3.0),
          (q(nkat), 3e9),
          (q(U), 3*60e6)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
