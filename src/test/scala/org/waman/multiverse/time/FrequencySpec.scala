package org.waman.multiverse.time

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FrequencySpec
  extends AbstractQuantityAndUnitSpec[FrequencyUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[FrequencyUnit]

  "Frequency object should be converted to a frequency by toFrequency method" in {
    val conversions =
      Table(
        ("frequency", "expected"),
        (3.0 Hz, 3.0 * 2.0 * Math.PI),
        (3.0 GHz, 3.0 * 1e9 * 2.0 * Math.PI)
      )
    __Verify__
    forAll(conversions){ (f: Frequency[Double], expected: Double) =>
      f.toAngularVelocity.rad/s should equal (%(expected))
    }
  }

  "Predefined frequency units" - {

    "3.0 <<frequency unit>> should be converted to the equivalent value in Heltz" in {
      __Exercise__
      val conversions =
        Table(
          ("frequencies", "expected"),
          (Seq(3.0.yHz, 3.0 yHz, 3.0 (yHz)), 3e-24),
          (Seq(3.0.zHz, 3.0 zHz, 3.0 (zHz)), 3e-21),
          (Seq(3.0.aHz, 3.0 aHz, 3.0 (aHz)), 3e-18),
          (Seq(3.0.fHz, 3.0 fHz, 3.0 (fHz)), 3e-15),
          (Seq(3.0.pHz, 3.0 pHz, 3.0 (pHz)), 3e-12),
          (Seq(3.0.nHz, 3.0 nHz, 3.0 (nHz)), 3e-9),
          (Seq(3.0.microHeltz, 3.0 microHeltz, 3.0 (microHeltz)), 3e-6),
          (Seq(3.0.microHz, 3.0 microHz, 3.0 (microHz)), 3e-6),
          (Seq(3.0.μHz, 3.0 μHz, 3.0 (μHz)), 3e-6),
          (Seq(3.0.mHz, 3.0 mHz, 3.0 (mHz)), 3e-3),
          (Seq(3.0.cHz, 3.0 cHz, 3.0 (cHz)), 3e-2),
          (Seq(3.0.dHz, 3.0 dHz, 3.0 (dHz)), 3e-1),
          (Seq(3.0.Hz, 3.0 Hz, 3.0 (Hz)), 3.0),
          (Seq(3.0.daHz, 3.0 daHz, 3.0 (daHz)), 3e1),
          (Seq(3.0.hHz, 3.0 hHz, 3.0 (hHz)), 3e2),
          (Seq(3.0.kHz, 3.0 kHz, 3.0 (kHz)), 3e3),
          (Seq(3.0.MHz, 3.0 MHz, 3.0 (MHz)), 3e6),
          (Seq(3.0.GHz, 3.0 GHz, 3.0 (GHz)), 3e9),
          (Seq(3.0.THz, 3.0 THz, 3.0 (THz)), 3e12),
          (Seq(3.0.PHz, 3.0 PHz, 3.0 (PHz)), 3e15),
          (Seq(3.0.EHz, 3.0 EHz, 3.0 (EHz)), 3e18),
          (Seq(3.0.ZHz, 3.0 ZHz, 3.0 (ZHz)), 3e21),
          (Seq(3.0.YHz, 3.0 YHz, 3.0 (YHz)), 3e24)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Frequency[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut Hz) should equal(%%%%(expected))
        }
      }
    }

    "3.0 Hz should be converted to the equivalent value in other frequency units" in {
      __SetUp__
      val q = 3.0 (Hz)
      __Exercise__
      val conversions =
        Table(
          ("frequencies", "expected"),
          (Seq(q.yHz, q yHz, q(yHz)), 3e24),
          (Seq(q.zHz, q zHz, q(zHz)), 3e21),
          (Seq(q.aHz, q aHz, q(aHz)), 3e18),
          (Seq(q.fHz, q fHz, q(fHz)), 3e15),
          (Seq(q.pHz, q pHz, q(pHz)), 3e12),
          (Seq(q.nHz, q nHz, q(nHz)), 3e9),
          (Seq(q.microHeltz, q microHeltz, q(microHeltz)), 3e6),
          (Seq(q.microHz, q microHz, q(microHz)), 3e6),
          (Seq(q.μHz, q μHz, q(μHz)), 3e6),
          (Seq(q.mHz, q mHz, q(mHz)), 3e3),
          (Seq(q.cHz, q cHz, q(cHz)), 3e2),
          (Seq(q.dHz, q dHz, q(dHz)), 3e1),
          (Seq(q.Hz, q Hz, q(Hz)), 3.0),
          (Seq(q.daHz, q daHz, q(daHz)), 3e-1),
          (Seq(q.hHz, q hHz, q(hHz)), 3e-2),
          (Seq(q.kHz, q kHz, q(kHz)), 3e-3),
          (Seq(q.MHz, q MHz, q(MHz)), 3e-6),
          (Seq(q.GHz, q GHz, q(GHz)), 3e-9),
          (Seq(q.THz, q THz, q(THz)), 3e-12),
          (Seq(q.PHz, q PHz, q(PHz)), 3e-15),
          (Seq(q.EHz, q EHz, q(EHz)), 3e-18),
          (Seq(q.ZHz, q ZHz, q(ZHz)), 3e-21),
          (Seq(q.YHz, q YHz, q(YHz)), 3e-24)
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
