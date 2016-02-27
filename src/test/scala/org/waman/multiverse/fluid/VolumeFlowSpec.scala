package org.waman.multiverse.fluid

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VolumeFlowSpec
  extends AbstractQuantityAndUnitSpec[VolumeFlowUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[VolumeFlowUnit]

  "VolumeFlowUnit should" - {

    "return a volumeFlow value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
      __SetUp__
      val vf = L/minute
      __Verify__
      vf.unitInCubicMetrePerSecond should equal (r"1e-3" / r"60")
    }

    "be evaluated as equal even if a different object" in {
      __Verify__
      (m3/s) should equal (m3/s)
      (m3/s).hashCode should equal ((m3/s).hashCode)
    }
  }

  "Predefined volume flow units" - {

    "3.0 <<volume flow unit>> should be converted to the equivalent value in cubic metre per second" in {
      __Exercise__
      val conversions =
        Table(
          ("volume flows", "expected"),
          (Seq(3.0.m3 / s, 3.0 m3 / s, 3.0 (m3 / s)), 3.0),
          (Seq(3.0.LPM, 3.0 LPM, 3.0 (LPM)), 3.0 * 1e-3 / 60.0)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[VolumeFlow[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut m3 / s) should equal(%%%%(expected))
        }
      }
    }

    "3.0 m3/s should be converted to the equivalent value in other volume flow units" in {
      __SetUp__
      val q = 3.0 (m3 / s)
      __Exercise__
      val conversions =
        Table(
          ("volume flows", "expected"),
          (Seq(q.m3 / s, q m3 / s, q(m3 / s)), 3.0),
          (Seq(q.LPM, q LPM, q(LPM)), 3.0 * 60.0 / 1e-3)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }
}
