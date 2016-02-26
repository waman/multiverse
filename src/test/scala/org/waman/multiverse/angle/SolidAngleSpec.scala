package org.waman.multiverse.angle

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class SolidAngleSpec
  extends AbstractQuantityAndUnitSpec[SolidAngleUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[SolidAngleUnit]

  "Tests where converting from some units to radian like 3.0 deg2 => 3.0 * (2 PI / 360)**2 sr" in {
    __Exercise__
    val conversions =
      Table(
        ("solid angles", "expected"),
        (Seq(3.0.dasr, 3.0 dasr, 3.0 (dasr)), 3e1),
        (Seq(3.0.sr, 3.0 sr, 3.0 (sr)), 3.0),
        (Seq(3.0.dsr, 3.0 dsr, 3.0 (dsr)), 3e-1),
        (Seq(3.0.csr, 3.0 csr, 3.0 (csr)), 3e-2),
        (Seq(3.0.msr, 3.0 msr, 3.0 (msr)), 3e-3),
        (Seq(3.0.μsr, 3.0 μsr, 3.0 (μsr)), 3e-6),
        (Seq(3.0.nsr, 3.0 nsr, 3.0 (nsr)), 3e-9),
        (Seq(3.0.psr, 3.0 psr, 3.0 (psr)), 3e-12),
        (Seq(3.0.fsr, 3.0 fsr, 3.0 (fsr)), 3e-15),
        (Seq(3.0.asr, 3.0 asr, 3.0 (asr)), 3e-18),
        (Seq(3.0.zsr, 3.0 zsr, 3.0 (zsr)), 3e-21),
        (Seq(3.0.ysr, 3.0 ysr, 3.0 (ysr)), 3e-24),

        (Seq(3.0.deg2, 3.0 deg2, 3.0 (deg2)), 3.0 * Math.pow(Math.PI / 180.0, 2))
      )
    __Exercise__
    forAll(conversions){ (suts: Seq[SolidAngle[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut sr) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting radian unit to other units like 3.0 rad => 3.0 * (360 / 2 * PI)**2 deg" in {
    __SetUp__
    val value = 3.0 (sr)
    __Exercise__
    val conversions =
      Table(
        ("solid angles", "expected"),
        (Seq(value.dasr, value dasr, value (dasr)), 3e-1),
        (Seq(value.sr, value sr, value (sr)), 3.0),
        (Seq(value.dsr, value dsr, value (dsr)), 3e1),
        (Seq(value.csr, value csr, value (csr)), 3e2),
        (Seq(value.msr, value msr, value (msr)), 3e3),
        (Seq(value.μsr, value μsr, value (μsr)), 3e6),
        (Seq(value.nsr, value nsr, value (nsr)), 3e9),
        (Seq(value.psr, value psr, value (psr)), 3e12),
        (Seq(value.fsr, value fsr, value (fsr)), 3e15),
        (Seq(value.asr, value asr, value (asr)), 3e18),
        (Seq(value.zsr, value zsr, value (zsr)), 3e21),
        (Seq(value.ysr, value ysr, value (ysr)), 3e24),

        (Seq(value.deg2, value deg2, value (deg2)), 3.0 * Math.pow(180.0 / Math.PI, 2))
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}