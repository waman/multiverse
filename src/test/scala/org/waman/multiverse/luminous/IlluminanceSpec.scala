package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class IlluminanceSpec
  extends AbstractQuantityAndUnitSpec[IlluminanceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[IlluminanceUnit]

  "3.0 <<illuminance unit>> should be converted to the equivalent value in Lux" in {
    __Exercise__
    val conversions =
      Table(
        ("illuminances", "expected"),
        (Seq(3.0.lx, 3.0 lx, 3.0 (lx)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Illuminance[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut lx) should equal (%%%%(expected))
      }
    }
  }

  "3.0 lx should be converted to the equivalent value in other illuminance units" in {
    __SetUp__
    val q = 3.0 (lx)
    __Exercise__
    val conversions =
      Table(
        ("illuminances", "expected"),
        (Seq(q.lx, q lx, q (lx)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
