package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FluxDensitySpec
  extends AbstractQuantityAndUnitSpec[FluxDensityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[FluxDensityUnit]

  "Predefined flux density units" - {
    
    "3.0 <<flux density unit>> should be converted to the equivalent value in Tesla" in {
      __Exercise__
      val conversions =
        Table(
          ("flux densities", "expected"),
          (Seq(3.0.yT, 3.0 yT, 3.0 (yT)), 3e-24),
          (Seq(3.0.zT, 3.0 zT, 3.0 (zT)), 3e-21),
          (Seq(3.0.aT, 3.0 aT, 3.0 (aT)), 3e-18),
          (Seq(3.0.fT, 3.0 fT, 3.0 (fT)), 3e-15),
          (Seq(3.0.pT, 3.0 pT, 3.0 (pT)), 3e-12),
          (Seq(3.0.nT, 3.0 nT, 3.0 (nT)), 3e-9),
          (Seq(3.0.microTesla, 3.0 microTesla, 3.0 (microTesla)), 3e-6),
          (Seq(3.0.microT, 3.0 microT, 3.0 (microT)), 3e-6),
          (Seq(3.0.μT, 3.0 μT, 3.0 (μT)), 3e-6),
          (Seq(3.0.mT, 3.0 mT, 3.0 (mT)), 3e-3),
          (Seq(3.0.cT, 3.0 cT, 3.0 (cT)), 3e-2),
          (Seq(3.0.dT, 3.0 dT, 3.0 (dT)), 3e-1),
          (Seq(3.0.T, 3.0 T, 3.0 (T)), 3.0),
          (Seq(3.0.daT, 3.0 daT, 3.0 (daT)), 3e1),
          (Seq(3.0.hT, 3.0 hT, 3.0 (hT)), 3e2),
          (Seq(3.0.kT, 3.0 kT, 3.0 (kT)), 3e3),
          (Seq(3.0.MT, 3.0 MT, 3.0 (MT)), 3e6),
          (Seq(3.0.GT, 3.0 GT, 3.0 (GT)), 3e9),
          (Seq(3.0.TT, 3.0 TT, 3.0 (TT)), 3e12),
          (Seq(3.0.PT, 3.0 PT, 3.0 (PT)), 3e15),
          (Seq(3.0.ET, 3.0 ET, 3.0 (ET)), 3e18),
          (Seq(3.0.ZT, 3.0 ZT, 3.0 (ZT)), 3e21),
          (Seq(3.0.YT, 3.0 YT, 3.0 (YT)), 3e24),

          (Seq(3.0.yG, 3.0 yG, 3.0 (yG)), 3e-28),
          (Seq(3.0.zG, 3.0 zG, 3.0 (zG)), 3e-25),
          (Seq(3.0.aG, 3.0 aG, 3.0 (aG)), 3e-22),
          (Seq(3.0.fG, 3.0 fG, 3.0 (fG)), 3e-19),
          (Seq(3.0.pG, 3.0 pG, 3.0 (pG)), 3e-16),
          (Seq(3.0.nG, 3.0 nG, 3.0 (nG)), 3e-13),
          (Seq(3.0.microGauss, 3.0 microGauss, 3.0 (microGauss)), 3e-10),
          (Seq(3.0.microG, 3.0 microG, 3.0 (microG)), 3e-10),
          (Seq(3.0.μG, 3.0 μG, 3.0 (μG)), 3e-10),
          (Seq(3.0.mG, 3.0 mG, 3.0 (mG)), 3e-7),
          (Seq(3.0.cG, 3.0 cG, 3.0 (cG)), 3e-6),
          (Seq(3.0.dG, 3.0 dG, 3.0 (dG)), 3e-5),
          (Seq(3.0.G, 3.0 G, 3.0 (G)), 3e-4),
          (Seq(3.0.daG, 3.0 daG, 3.0 (daG)), 3e-3),
          (Seq(3.0.hG, 3.0 hG, 3.0 (hG)), 3e-2),
          (Seq(3.0.kG, 3.0 kG, 3.0 (kG)), 3e-1),
          (Seq(3.0.MG, 3.0 MG, 3.0 (MG)), 3e2),
          (Seq(3.0.GG, 3.0 GG, 3.0 (GG)), 3e5),
          (Seq(3.0.TG, 3.0 TG, 3.0 (TG)), 3e8),
          (Seq(3.0.PG, 3.0 PG, 3.0 (PG)), 3e11),
          (Seq(3.0.EG, 3.0 EG, 3.0 (EG)), 3e14),
          (Seq(3.0.ZG, 3.0 ZG, 3.0 (ZG)), 3e17),
          (Seq(3.0.YG, 3.0 YG, 3.0 (YG)), 3e20)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[FluxDensity[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut T) should equal(%%%%(expected))
        }
      }
    }

    "3.0 T should be converted to the equivalent value in other flux density units" in {
      __SetUp__
      val q = 3.0 (T)
      __Exercise__
      val conversions =
        Table(
          ("flux densities", "expected"),
          (Seq(q.yT, q yT, q(yT)), 3e24),
          (Seq(q.zT, q zT, q(zT)), 3e21),
          (Seq(q.aT, q aT, q(aT)), 3e18),
          (Seq(q.fT, q fT, q(fT)), 3e15),
          (Seq(q.pT, q pT, q(pT)), 3e12),
          (Seq(q.nT, q nT, q(nT)), 3e9),
          (Seq(q.microTesla, q microTesla, q(microTesla)), 3e6),
          (Seq(q.microT, q microT, q(microT)), 3e6),
          (Seq(q.μT, q μT, q(μT)), 3e6),
          (Seq(q.mT, q mT, q(mT)), 3e3),
          (Seq(q.cT, q cT, q(cT)), 3e2),
          (Seq(q.dT, q dT, q(dT)), 3e1),
          (Seq(q.T, q T, q(T)), 3.0),
          (Seq(q.daT, q daT, q(daT)), 3e-1),
          (Seq(q.hT, q hT, q(hT)), 3e-2),
          (Seq(q.kT, q kT, q(kT)), 3e-3),
          (Seq(q.MT, q MT, q(MT)), 3e-6),
          (Seq(q.GT, q GT, q(GT)), 3e-9),
          (Seq(q.TT, q TT, q(TT)), 3e-12),
          (Seq(q.PT, q PT, q(PT)), 3e-15),
          (Seq(q.ET, q ET, q(ET)), 3e-18),
          (Seq(q.ZT, q ZT, q(ZT)), 3e-21),
          (Seq(q.YT, q YT, q(YT)), 3e-24),

          (Seq(q.yG, q yG, q(yG)), 3e28),
          (Seq(q.zG, q zG, q(zG)), 3e25),
          (Seq(q.aG, q aG, q(aG)), 3e22),
          (Seq(q.fG, q fG, q(fG)), 3e19),
          (Seq(q.pG, q pG, q(pG)), 3e16),
          (Seq(q.nG, q nG, q(nG)), 3e13),
          (Seq(q.microGauss, q microGauss, q(microGauss)), 3e10),
          (Seq(q.microG, q microG, q(microG)), 3e10),
          (Seq(q.μG, q μG, q(μG)), 3e10),
          (Seq(q.mG, q mG, q(mG)), 3e7),
          (Seq(q.cG, q cG, q(cG)), 3e6),
          (Seq(q.dG, q dG, q(dG)), 3e5),
          (Seq(q.G, q G, q(G)), 3e4),
          (Seq(q.daG, q daG, q(daG)), 3e3),
          (Seq(q.hG, q hG, q(hG)), 3e2),
          (Seq(q.kG, q kG, q(kG)), 3e1),
          (Seq(q.MG, q MG, q(MG)), 3e-2),
          (Seq(q.GG, q GG, q(GG)), 3e-5),
          (Seq(q.TG, q TG, q(TG)), 3e-8),
          (Seq(q.PG, q PG, q(PG)), 3e-11),
          (Seq(q.EG, q EG, q(EG)), 3e-14),
          (Seq(q.ZG, q ZG, q(ZG)), 3e-17),
          (Seq(q.YG, q YG, q(YG)), 3e-20)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Quotient flux density unit" - {

    "Inductance unit of hWb/mm2 should equal 1e8 T" in {
      __Exercise__
      val sut = hWb/mm2
      __Verify__
      sut.unitInTesla.toDouble should equal (%%%%(1e8))
    }

    "3.0 hWb/mm2 should equal 3e8 T" in {
      __Exercise__
      val conversions =
        Table(
          ("flux density", "expected"),
          (3.0.hWb/mm2, 3e8),
          (3.0 hWb/mm2, 3e8),
          (3.0 (hWb/mm2), 3e8)
        )
      __Verify__
      forAll(conversions){ (sut: FluxDensity[Double], expected: Double) =>
        sut.T should equal (%%%%(expected))
      }
    }

    "3.0 T should equal 3e-8 hWb/mm2" in {
      __SetUp__
      val q = 3.0 (T)
      val expected = 3e-8
      __Exercise__
      val conversions =
        Table(
          ("flux density", "expected"),
          (q.hWb/mm2, expected),
          (q hWb/mm2, expected),
          (q (hWb/mm2), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
