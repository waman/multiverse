package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class DipoleSpec
  extends AbstractQuantityAndUnitSpec[DipoleUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[DipoleUnit]

  "3.0 <<dipole unit>> should be converted to the equivalent value in Coulomb metre" in {
    __Exercise__
    val conversions =
      Table(
        ("dipoles", "expected"),
        (Seq(3.0.C*m, 3.0 C*m, 3.0 (C*m)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Dipole[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut C*m) should equal (%%%%(expected))
      }
    }
  }

  "3.0 m should be converted to the equivalent value in other dipole units" in {
    __SetUp__
    val q = 3.0 (C*m)
    __Exercise__
    val conversions =
      Table(
        ("dipoles", "expected"),
        (Seq(q.C*m, q C*m, q (C*m)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
