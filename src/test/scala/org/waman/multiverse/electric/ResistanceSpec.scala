package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ResistanceSpec
  extends AbstractQuantityAndUnitSpec[ResistanceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[ResistanceUnit]

  "3.0 <<resistance unit>> should be converted to the equivalent value in Ω" in {
    __Exercise__
    val conversions =
      Table(
        ("resistances", "expected"),
//        (Seq(3.0.ohm, 3.0 ohm, 3.0 (ohm)), 3.0),  // TODO
        (Seq(3.0.Ω, 3.0 Ω, 3.0 (Ω)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Resistance[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Ω) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Ω should be converted to the equivalent value in other resistance units" in {
    __SetUp__
    val q = 3.0 (Ω)
    __Exercise__
    val conversions =
      Table(
        ("resistances", "expected"),
//        (Seq(q.ohm, q ohm, q (ohm)), 3.0), // TODO
        (Seq(q.Ω, q Ω, q (Ω)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
