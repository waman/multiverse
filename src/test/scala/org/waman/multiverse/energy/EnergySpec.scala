package org.waman.multiverse.energy

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EnergySpec
  extends AbstractQuantityAndUnitSpec[EnergyUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[EnergyUnit]

  "3.0 <<energy unit>> should be converted to the equivalent value in Joule" in {
    __Exercise__
    val conversions =
      Table(
        ("energies", "expected"),
        (Seq(3.0.J, 3.0 J, 3.0 (J)), 3.0),

        (Seq(3.0.eV, 3.0 eV, 3.0 (eV)), 3.0 * 1.602176620898e-19)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Energy[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut J) should equal (%%%%(expected))
      }
    }
  }

  "3.0 J should be converted to the equivalent value in other energy units" in {
    __SetUp__
    val q = 3.0 (J)
    __Exercise__
    val conversions =
      Table(
        ("energies", "expected"),
        (Seq(q.J, q J, q (J)), 3.0),
        (Seq(q.eV, q eV, q (eV)), 3.0 / 1.602176620898e-19)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
