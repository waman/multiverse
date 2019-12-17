package org.waman.multiverse.unit.electric

import spire.implicits._

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.electric
import electric.ChargeUnitObjects.statcoulomb
import electric.DipoleUnitObjects.debye
import electric.ChargeUnits._
import org.waman.multiverse.unit.basic.LengthUnits._

class DipoleSpec extends MultiverseCustomSpec {

  "Unit" - {

    "Dimension of a product of units should equal the dimension of the corresponding product unit" in {
      // SetUp
      val expected = DipoleUnit.dimension
      // Exercise
      val sut = (C*m).dimension
      // Verify
      sut should contain theSameElementsAs expected
    }
  }

  "Quantity" - {

    "Multipliation between quantities should work well" in {
      // SetUp
      val q = 6(C)
      val x = 3(m)
      val expected = 18(C*m)
      // Exercise
      val sut = q*x
      // Verify
      sut should equal (expected)
    }
  }

  "[SOURCE GENERATION] " - {

    "The unit value of stat should be collect value (the interval string is a product of constant and number)" in {
      // Exercise
      val sut = debye.interval
      // Verify
      sut should equal (r"1e-20" * statcoulomb.interval)
    }

    "Product unit 'statC*NM' should have the combinated aliases" in {
      // SetUp
      val expected = Seq("statC*nmi", "Fr*NM", "Fr*nmi", "esu*NM", "esu*nmi")
      // Exercise
      val sut = statC * NM
      // Verify
      sut.aliases should contain theSameElementsAs expected
      sut.symbol should be ("statC*NM")
    }
  }
}
