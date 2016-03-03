package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AbsorbedDoseSpec
  extends AbstractQuantityAndUnitSpec[AbsorbedDoseUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[AbsorbedDoseUnit]

  "Predefined absorbed dose units" - {

    "3.0 <<absorbed dose unit>> should be converted to the equivalent value in Gray" in {
      __Exercise__
      val conversions =
        Table(
          ("absorbedDoses", "expected"),
          (Seq(3.0.yGy, 3.0 yGy, 3.0 (yGy)), 3e-24),
          (Seq(3.0.zGy, 3.0 zGy, 3.0 (zGy)), 3e-21),
          (Seq(3.0.aGy, 3.0 aGy, 3.0 (aGy)), 3e-18),
          (Seq(3.0.fGy, 3.0 fGy, 3.0 (fGy)), 3e-15),
          (Seq(3.0.pGy, 3.0 pGy, 3.0 (pGy)), 3e-12),
          (Seq(3.0.nGy, 3.0 nGy, 3.0 (nGy)), 3e-9),
          (Seq(3.0.microGray, 3.0 microGray, 3.0 (microGray)), 3e-6),
          (Seq(3.0.microGy, 3.0 microGy, 3.0 (microGy)), 3e-6),
          (Seq(3.0.μGy, 3.0 μGy, 3.0 (μGy)), 3e-6),
          (Seq(3.0.mGy, 3.0 mGy, 3.0 (mGy)), 3e-3),
          (Seq(3.0.cGy, 3.0 cGy, 3.0 (cGy)), 3e-2),
          (Seq(3.0.dGy, 3.0 dGy, 3.0 (dGy)), 3e-1),
          (Seq(3.0.Gy, 3.0 Gy, 3.0 (Gy)), 3.0),
          (Seq(3.0.daGy, 3.0 daGy, 3.0 (daGy)), 3e1),
          (Seq(3.0.hGy, 3.0 hGy, 3.0 (hGy)), 3e2),
          (Seq(3.0.kGy, 3.0 kGy, 3.0 (kGy)), 3e3),
          (Seq(3.0.MGy, 3.0 MGy, 3.0 (MGy)), 3e6),
          (Seq(3.0.GGy, 3.0 GGy, 3.0 (GGy)), 3e9),
          (Seq(3.0.TGy, 3.0 TGy, 3.0 (TGy)), 3e12),
          (Seq(3.0.PGy, 3.0 PGy, 3.0 (PGy)), 3e15),
          (Seq(3.0.EGy, 3.0 EGy, 3.0 (EGy)), 3e18),
          (Seq(3.0.ZGy, 3.0 ZGy, 3.0 (ZGy)), 3e21),
          (Seq(3.0.YGy, 3.0 YGy, 3.0 (YGy)), 3e24)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[AbsorbedDose[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut Gy) should equal(%%%%(expected))
        }
      }
    }

    "3.0 Gy should be converted to the equivalent value in other absorbed dose units" in {
      __SetUp__
      val q = 3.0 (Gy)
      __Exercise__
      val conversions =
        Table(
          ("absorbedDoses", "expected"),
          (Seq(q.yGy, q yGy, q(yGy)), 3e24),
          (Seq(q.zGy, q zGy, q(zGy)), 3e21),
          (Seq(q.aGy, q aGy, q(aGy)), 3e18),
          (Seq(q.fGy, q fGy, q(fGy)), 3e15),
          (Seq(q.pGy, q pGy, q(pGy)), 3e12),
          (Seq(q.nGy, q nGy, q(nGy)), 3e9),
          (Seq(q.microGray, q microGray, q(microGray)), 3e6),
          (Seq(q.microGy, q microGy, q(microGy)), 3e6),
          (Seq(q.μGy, q μGy, q(μGy)), 3e6),
          (Seq(q.mGy, q mGy, q(mGy)), 3e3),
          (Seq(q.cGy, q cGy, q(cGy)), 3e2),
          (Seq(q.dGy, q dGy, q(dGy)), 3e1),
          (Seq(q.Gy, q Gy, q(Gy)), 3.0),
          (Seq(q.daGy, q daGy, q(daGy)), 3e-1),
          (Seq(q.hGy, q hGy, q(hGy)), 3e-2),
          (Seq(q.kGy, q kGy, q(kGy)), 3e-3),
          (Seq(q.MGy, q MGy, q(MGy)), 3e-6),
          (Seq(q.GGy, q GGy, q(GGy)), 3e-9),
          (Seq(q.TGy, q TGy, q(TGy)), 3e-12),
          (Seq(q.PGy, q PGy, q(PGy)), 3e-15),
          (Seq(q.EGy, q EGy, q(EGy)), 3e-18),
          (Seq(q.ZGy, q ZGy, q(ZGy)), 3e-21),
          (Seq(q.YGy, q YGy, q(YGy)), 3e-24)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Quotient absorbed dose unit" - {

    "Absorbed dose unit of J/kg should equal 1.0 Gy" in {
      __Exercise__
      val sut = J/kg
      __Verify__
      sut.unitInGray.toDouble should equal (%%%%(1.0))
    }

    "3.0 J/kg should equal 3.0 Gy" in {
      __Exercise__
      val conversions =
        Table(
          ("absorbed dose", "expected"),
          (3.0.J/kg, 3.0),
          (3.0 J/kg, 3.0),
          (3.0 (J/kg), 3.0)
        )
      __Verify__
      forAll(conversions){ (sut: AbsorbedDose[Double], expected: Double) =>
        sut.Gy should equal (%%%%(expected))
      }
    }

    "3.0 Gy should equal 3.0 J/kg" in {
      __SetUp__
      val q = 3.0 (Gy)
      val expected = 3.0
      __Exercise__
      val conversions =
        Table(
          ("absorbed dose", "expected"),
          (q.J/kg, expected),
          (q J/kg, expected),
          (q (J/kg), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
