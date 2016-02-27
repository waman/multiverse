package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class CapacitanceSpec
  extends AbstractQuantityAndUnitSpec[CapacitanceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[CapacitanceUnit]

  "3.0 <<capacitance unit>> should be converted to the equivalent value in Frad" in {
    __Exercise__
    val conversions =
      Table(
        ("capacitances", "expected"),
        (Seq(3.0.F, 3.0 F, 3.0 (F)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Capacitance[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut F) should equal (%%%%(expected))
      }
    }
  }

  "3.0 F should be converted to the equivalent value in other capacitance units" in {
    __SetUp__
    val q = 3.0 (F)
    __Exercise__
    val conversions =
      Table(
        ("capacitances", "expected"),
        (Seq(q.F, q F, q (F)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
