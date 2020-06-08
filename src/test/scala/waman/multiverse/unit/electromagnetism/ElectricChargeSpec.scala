package waman.multiverse.unit.electromagnetism

import waman.multiverse.MultiverseCustomSpec
import waman.multiverse.implicits._
import waman.multiverse.unit.BasicUnits._
import waman.multiverse.unit.Constants
import waman.multiverse.unit.electromagnetism.ElectricChargeUnits._
import waman.multiverse.unit.electromagnetism.ElectricCurrentUnits._

class ElectricChargeSpec extends MultiverseCustomSpec {

  val faradUnit: Double = Constants.ElementaryCharge.toDouble * Constants.AvogadroConstant.toDouble
  val esuUnit: Double = 0.1 / Constants.SpeedOfLight.toDouble

  "Quantity" - {

    "3.0 <<charge unit>> should be converted to the equivalent value in ampare" in {
      // Exercise
      val conversions =
        Table(
          ("charge", "expected"),
          (3.0(abC), 3.0 * 10.0),
          (3.0(emu), 3.0 * 10.0),
          (3.0(au), 3.0 * Constants.ElementaryCharge.toDouble),
          (3.0(C), 3.0),
          (3.0(F), 3.0 * faradUnit),
          (3.0(mA*h), 3.0 * 3.6),
          (3.0(statC), 3.0 * esuUnit),
          (3.0(Fr), 3.0 * esuUnit),
          (3.0(esu), 3.0 * esuUnit)
        )
      // Verify
      forAll(conversions) { (sut: ElectricCharge[Double], expected: Double) =>
        sut(C) should equal(%%%%(expected))
      }
    }

    "3.0(A) should be converted to the equivalent value in other charge units" in {
      // SetUp
      val q = 3.0(C)
      // Exercise
      val conversions =
        Table(
          ("charge", "expected"),
          (q(abC), 3.0 / 10.0),
          (q(emu), 3.0 / 10.0),
          (q(au), 3.0 / Constants.ElementaryCharge.toDouble),
          (q(C), 3.0),
          (q(F), 3.0 / faradUnit),
          (q(mA*h), 3.0 / 3.6),
          (q(statC), 3.0 / esuUnit),
          (q(Fr), 3.0 / esuUnit),
          (q(esu), 3.0 / esuUnit)
        )
      // Verify
      forAll(conversions) { (sut: Double, expected: Double) =>
        sut should equal(%%%%(expected))
      }
    }
  }
}
