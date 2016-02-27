package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VoltageSpec
  extends AbstractQuantityAndUnitSpec[VoltageUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[VoltageUnit]

  "3.0 <<voltage unit>> should be converted to the equivalent value in Volt" in {
    __Exercise__
    val conversions =
      Table(
        ("voltages", "expected"),
        (Seq(3.0.V, 3.0 V, 3.0 (V)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Voltage[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut V) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Volt should be converted to the equivalent value in other voltage units" in {
    __SetUp__
    val q = 3.0 (V)
    __Exercise__
    val conversions =
      Table(
        ("voltages", "expected"),
        (Seq(q.V, q V, q (V)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
