package org.waman.multiverse.magnetic

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class InductanceSpec
  extends AbstractQuantityAndUnitSpec[InductanceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[InductanceUnit]

  "Predefined inductance units" - {
    
    "3.0 <<inductance unit>> should be converted to the equivalent value in Henry" in {
      __Exercise__
      val conversions =
        Table(
          ("inductances", "expected"),
          (Seq(3.0.yH, 3.0 yH, 3.0 (yH)), 3e-24),
          (Seq(3.0.zH, 3.0 zH, 3.0 (zH)), 3e-21),
          (Seq(3.0.aH, 3.0 aH, 3.0 (aH)), 3e-18),
          (Seq(3.0.fH, 3.0 fH, 3.0 (fH)), 3e-15),
          (Seq(3.0.pH, 3.0 pH, 3.0 (pH)), 3e-12),
          (Seq(3.0.nH, 3.0 nH, 3.0 (nH)), 3e-9),
          (Seq(3.0.μH, 3.0 μH, 3.0 (μH)), 3e-6),
          (Seq(3.0.mcH, 3.0 mcH, 3.0 (mcH)), 3e-6),
          (Seq(3.0.mH, 3.0 mH, 3.0 (mH)), 3e-3),
          (Seq(3.0.cH, 3.0 cH, 3.0 (cH)), 3e-2),
          (Seq(3.0.dH, 3.0 dH, 3.0 (dH)), 3e-1),
          (Seq(3.0.H, 3.0 H, 3.0 (H)), 3.0),
          (Seq(3.0.daH, 3.0 daH, 3.0 (daH)), 3e1),
          (Seq(3.0.hH, 3.0 hH, 3.0 (hH)), 3e2),
          (Seq(3.0.kH, 3.0 kH, 3.0 (kH)), 3e3),
          (Seq(3.0.MH, 3.0 MH, 3.0 (MH)), 3e6),
          (Seq(3.0.GH, 3.0 GH, 3.0 (GH)), 3e9),
          (Seq(3.0.TH, 3.0 TH, 3.0 (TH)), 3e12),
          (Seq(3.0.PH, 3.0 PH, 3.0 (PH)), 3e15),
          (Seq(3.0.EH, 3.0 EH, 3.0 (EH)), 3e18),
          (Seq(3.0.ZH, 3.0 ZH, 3.0 (ZH)), 3e21),
          (Seq(3.0.YH, 3.0 YH, 3.0 (YH)), 3e24)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Inductance[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut H) should equal(%%%%(expected))
        }
      }
    }

    "3.0 H should be converted to the equivalent value in other inductance units" in {
      __SetUp__
      val q = 3.0 (H)
      __Exercise__
      val conversions =
        Table(
          ("inductances", "expected"),
          (Seq(q.yH, q yH, q(yH)), 3e24),
          (Seq(q.zH, q zH, q(zH)), 3e21),
          (Seq(q.aH, q aH, q(aH)), 3e18),
          (Seq(q.fH, q fH, q(fH)), 3e15),
          (Seq(q.pH, q pH, q(pH)), 3e12),
          (Seq(q.nH, q nH, q(nH)), 3e9),
          (Seq(q.μH, q μH, q(μH)), 3e6),
          (Seq(q.mcH, q mcH, q(mcH)), 3e6),
          (Seq(q.mH, q mH, q(mH)), 3e3),
          (Seq(q.cH, q cH, q(cH)), 3e2),
          (Seq(q.dH, q dH, q(dH)), 3e1),
          (Seq(q.H, q H, q(H)), 3.0),
          (Seq(q.daH, q daH, q(daH)), 3e-1),
          (Seq(q.hH, q hH, q(hH)), 3e-2),
          (Seq(q.kH, q kH, q(kH)), 3e-3),
          (Seq(q.MH, q MH, q(MH)), 3e-6),
          (Seq(q.GH, q GH, q(GH)), 3e-9),
          (Seq(q.TH, q TH, q(TH)), 3e-12),
          (Seq(q.PH, q PH, q(PH)), 3e-15),
          (Seq(q.EH, q EH, q(EH)), 3e-18),
          (Seq(q.ZH, q ZH, q(ZH)), 3e-21),
          (Seq(q.YH, q YH, q(YH)), 3e-24)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Quotient inductance unit" - {

    "Inductance unit of hWb/mA should equal 1e5 H" in {
      __Exercise__
      val sut = hWb/mA
      __Verify__
      sut.unitValueInSIUnit.toDouble should equal (%%%%(1e5))
    }

    "3.0 hWb/mA should equal 3e5 H" in {
      __Exercise__
      val conversions =
        Table(
          ("inductance", "expected"),
          (3.0.hWb/mA, 3e5),
          (3.0 hWb/mA, 3e5),
          (3.0 (hWb/mA), 3e5)
        )
      __Verify__
      forAll(conversions){ (sut: Inductance[Double], expected: Double) =>
        sut.H should equal (%%%%(expected))
      }
    }

    "3.0 H should equal 3e-5 hWb/mA" in {
      __SetUp__
      val q = 3.0 (H)
      val expected = 3e-5
      __Exercise__
      val conversions =
        Table(
          ("inductance", "expected"),
          (q.hWb/mA, expected),
          (q hWb/mA, expected),
          (q (hWb/mA), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
