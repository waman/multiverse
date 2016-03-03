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

  "3.0 <<solid angle unit>> should be converted to the equivalent value in steradian" in {
    __Exercise__
    val conversions =
      Table(
        ("solid angles", "expected"),
        (Seq(3.0.dasr, 3.0 dasr, 3.0 (dasr)), 3e1),
        (Seq(3.0.sr, 3.0 sr, 3.0 (sr)), 3.0),
        (Seq(3.0.dsr, 3.0 dsr, 3.0 (dsr)), 3e-1),
        (Seq(3.0.csr, 3.0 csr, 3.0 (csr)), 3e-2),
        (Seq(3.0.msr, 3.0 msr, 3.0 (msr)), 3e-3),
        (Seq(3.0.microSteradian, 3.0 microSteradian, 3.0 (microSteradian)), 3e-6),
        (Seq(3.0.microSr, 3.0 microSr, 3.0 (microSr)), 3e-6),
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

  "3.0 sr should be converted to the equivalent value in other solid angle units" in {
    __SetUp__
    val q = 3.0 (sr)
    __Exercise__
    val conversions =
      Table(
        ("solid angles", "expected"),
        (Seq(q.dasr, q dasr, q (dasr)), 3e-1),
        (Seq(q.sr, q sr, q (sr)), 3.0),
        (Seq(q.dsr, q dsr, q (dsr)), 3e1),
        (Seq(q.csr, q csr, q (csr)), 3e2),
        (Seq(q.msr, q msr, q (msr)), 3e3),
        (Seq(q.microSteradian, q microSteradian, q (microSteradian)), 3e6),
        (Seq(q.microSr, q microSr, q (microSr)), 3e6),
        (Seq(q.μsr, q μsr, q (μsr)), 3e6),
        (Seq(q.nsr, q nsr, q (nsr)), 3e9),
        (Seq(q.psr, q psr, q (psr)), 3e12),
        (Seq(q.fsr, q fsr, q (fsr)), 3e15),
        (Seq(q.asr, q asr, q (asr)), 3e18),
        (Seq(q.zsr, q zsr, q (zsr)), 3e21),
        (Seq(q.ysr, q ysr, q (ysr)), 3e24),

        (Seq(q.deg2, q deg2, q (deg2)), 3.0 * Math.pow(180.0 / Math.PI, 2))
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}