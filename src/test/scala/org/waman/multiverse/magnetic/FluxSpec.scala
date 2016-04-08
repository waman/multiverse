package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FluxSpec
  extends AbstractQuantityAndUnitSpec[FluxUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[FluxUnit]

  "Predefined current units" - {
    
    "3.0 <<flux unit>> should be converted to the equivalent value in Weber" in {
      __Exercise__
      val conversions =
        Table(
          ("fluxes", "expected"),
          (Seq(3.0.yWb, 3.0 yWb, 3.0 (yWb)), 3e-24),
          (Seq(3.0.zWb, 3.0 zWb, 3.0 (zWb)), 3e-21),
          (Seq(3.0.aWb, 3.0 aWb, 3.0 (aWb)), 3e-18),
          (Seq(3.0.fWb, 3.0 fWb, 3.0 (fWb)), 3e-15),
          (Seq(3.0.pWb, 3.0 pWb, 3.0 (pWb)), 3e-12),
          (Seq(3.0.nWb, 3.0 nWb, 3.0 (nWb)), 3e-9),
          (Seq(3.0.μWb, 3.0 μWb, 3.0 (μWb)), 3e-6),
          (Seq(3.0.mcWb, 3.0 mcWb, 3.0 (mcWb)), 3e-6),
          (Seq(3.0.mWb, 3.0 mWb, 3.0 (mWb)), 3e-3),
          (Seq(3.0.cWb, 3.0 cWb, 3.0 (cWb)), 3e-2),
          (Seq(3.0.dWb, 3.0 dWb, 3.0 (dWb)), 3e-1),
          (Seq(3.0.Wb, 3.0 Wb, 3.0 (Wb)), 3.0),
          (Seq(3.0.daWb, 3.0 daWb, 3.0 (daWb)), 3e1),
          (Seq(3.0.hWb, 3.0 hWb, 3.0 (hWb)), 3e2),
          (Seq(3.0.kWb, 3.0 kWb, 3.0 (kWb)), 3e3),
          (Seq(3.0.MWb, 3.0 MWb, 3.0 (MWb)), 3e6),
          (Seq(3.0.GWb, 3.0 GWb, 3.0 (GWb)), 3e9),
          (Seq(3.0.TWb, 3.0 TWb, 3.0 (TWb)), 3e12),
          (Seq(3.0.PWb, 3.0 PWb, 3.0 (PWb)), 3e15),
          (Seq(3.0.EWb, 3.0 EWb, 3.0 (EWb)), 3e18),
          (Seq(3.0.ZWb, 3.0 ZWb, 3.0 (ZWb)), 3e21),
          (Seq(3.0.YWb, 3.0 YWb, 3.0 (YWb)), 3e24),

          (Seq(3.0.yMx, 3.0 yMx, 3.0 (yMx)), 3e-32),
          (Seq(3.0.zMx, 3.0 zMx, 3.0 (zMx)), 3e-29),
          (Seq(3.0.aMx, 3.0 aMx, 3.0 (aMx)), 3e-26),
          (Seq(3.0.fMx, 3.0 fMx, 3.0 (fMx)), 3e-23),
          (Seq(3.0.pMx, 3.0 pMx, 3.0 (pMx)), 3e-20),
          (Seq(3.0.nMx, 3.0 nMx, 3.0 (nMx)), 3e-17),
          (Seq(3.0.μMx, 3.0 μMx, 3.0 (μMx)), 3e-14),
          (Seq(3.0.mcMx, 3.0 mcMx, 3.0 (mcMx)), 3e-14),
          (Seq(3.0.mMx, 3.0 mMx, 3.0 (mMx)), 3e-11),
          (Seq(3.0.cMx, 3.0 cMx, 3.0 (cMx)), 3e-10),
          (Seq(3.0.dMx, 3.0 dMx, 3.0 (dMx)), 3e-9),
          (Seq(3.0.Mx, 3.0 Mx, 3.0 (Mx)), 3e-8),
          (Seq(3.0.daMx, 3.0 daMx, 3.0 (daMx)), 3e-7),
          (Seq(3.0.hMx, 3.0 hMx, 3.0 (hMx)), 3.0e-6),
          (Seq(3.0.kMx, 3.0 kMx, 3.0 (kMx)), 3e-5),
          (Seq(3.0.MMx, 3.0 MMx, 3.0 (MMx)), 3e-2),
          (Seq(3.0.GMx, 3.0 GMx, 3.0 (GMx)), 3e1),
          (Seq(3.0.TMx, 3.0 TMx, 3.0 (TMx)), 3e4),
          (Seq(3.0.PMx, 3.0 PMx, 3.0 (PMx)), 3e7),
          (Seq(3.0.EMx, 3.0 EMx, 3.0 (EMx)), 3e10),
          (Seq(3.0.ZMx, 3.0 ZMx, 3.0 (ZMx)), 3e13),
          (Seq(3.0.YMx, 3.0 YMx, 3.0 (YMx)), 3e16)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Flux[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut Wb) should equal(%%%%(expected))
        }
      }
    }

    "3.0 Wb should be converted to the equivalent value in other flux units" in {
      __SetUp__
      val q = 3.0 (Wb)
      __Exercise__
      val conversions =
        Table(
          ("fluxes", "expected"),
          (Seq(q.yWb, q yWb, q(yWb)), 3e24),
          (Seq(q.zWb, q zWb, q(zWb)), 3e21),
          (Seq(q.aWb, q aWb, q(aWb)), 3e18),
          (Seq(q.fWb, q fWb, q(fWb)), 3e15),
          (Seq(q.pWb, q pWb, q(pWb)), 3e12),
          (Seq(q.nWb, q nWb, q(nWb)), 3e9),
          (Seq(q.μWb, q μWb, q(μWb)), 3e6),
          (Seq(q.mcWb, q mcWb, q(mcWb)), 3e6),
          (Seq(q.mWb, q mWb, q(mWb)), 3e3),
          (Seq(q.cWb, q cWb, q(cWb)), 3e2),
          (Seq(q.dWb, q dWb, q(dWb)), 3e1),
          (Seq(q.Wb, q Wb, q(Wb)), 3.0),
          (Seq(q.daWb, q daWb, q(daWb)), 3e-1),
          (Seq(q.hWb, q hWb, q(hWb)), 3e-2),
          (Seq(q.kWb, q kWb, q(kWb)), 3e-3),
          (Seq(q.MWb, q MWb, q(MWb)), 3e-6),
          (Seq(q.GWb, q GWb, q(GWb)), 3e-9),
          (Seq(q.TWb, q TWb, q(TWb)), 3e-12),
          (Seq(q.PWb, q PWb, q(PWb)), 3e-15),
          (Seq(q.EWb, q EWb, q(EWb)), 3e-18),
          (Seq(q.ZWb, q ZWb, q(ZWb)), 3e-21),
          (Seq(q.YWb, q YWb, q(YWb)), 3e-24),

          (Seq(q.yMx, q yMx, q(yMx)), 3e32),
          (Seq(q.zMx, q zMx, q(zMx)), 3e29),
          (Seq(q.aMx, q aMx, q(aMx)), 3e26),
          (Seq(q.fMx, q fMx, q(fMx)), 3e23),
          (Seq(q.pMx, q pMx, q(pMx)), 3e20),
          (Seq(q.nMx, q nMx, q(nMx)), 3e17),
          (Seq(q.μMx, q μMx, q(μMx)), 3e14),
          (Seq(q.mcMx, q mcMx, q(mcMx)), 3e14),
          (Seq(q.mMx, q mMx, q(mMx)), 3e11),
          (Seq(q.cMx, q cMx, q(cMx)), 3e10),
          (Seq(q.dMx, q dMx, q(dMx)), 3e9),
          (Seq(q.Mx, q Mx, q(Mx)), 3e8),
          (Seq(q.daMx, q daMx, q(daMx)), 3e7),
          (Seq(q.hMx, q hMx, q(hMx)), 3e6),
          (Seq(q.kMx, q kMx, q(kMx)), 3e5),
          (Seq(q.MMx, q MMx, q(MMx)), 3e2),
          (Seq(q.GMx, q GMx, q(GMx)), 3e-1),
          (Seq(q.TMx, q TMx, q(TMx)), 3e-4),
          (Seq(q.PMx, q PMx, q(PMx)), 3e-7),
          (Seq(q.EMx, q EMx, q(EMx)), 3e-10),
          (Seq(q.ZMx, q ZMx, q(ZMx)), 3e-13),
          (Seq(q.YMx, q YMx, q(YMx)), 3e-16)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Product flux unit" - {

    "Flux unit of kV*h should equal 3600e3 Wb" in {
      __Exercise__
      val sut = kV*h
      __Verify__
      sut.unitValueInSIUnit.toDouble should equal (%%%%(3600e3))
    }

    "3.0 kV*h should equal 3.0 * 3600e3 Wb" in {
      __Exercise__
      val conversions =
        Table(
          ("flux", "expected"),
          (3.0.kV*h, 3.0 * 3600e3),
          (3.0 kV*h, 3.0 * 3600e3),
          (3.0 (kV*h), 3.0 * 3600e3)
        )
      __Verify__
      forAll(conversions){ (sut: Flux[Double], expected: Double) =>
        sut.Wb should equal (%%%%(expected))
      }
    }

    "3.0 Wb should equal 3.0 / 3600e3 kV*h" in {
      __SetUp__
      val q = 3.0 (Wb)
      val expected = 3.0 / 3600e3
      __Exercise__
      val conversions =
        Table(
          ("flux", "expected"),
          (q.kV*h, expected),
          (q kV*h, expected),
          (q (kV*h), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
