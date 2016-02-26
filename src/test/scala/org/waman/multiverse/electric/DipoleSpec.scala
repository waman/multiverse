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

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
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

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (C*m)
    __Exercise__
    val conversions =
      Table(
        ("dipoles", "expected"),
        (Seq(value.C*m, value C*m, value (C*m)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
