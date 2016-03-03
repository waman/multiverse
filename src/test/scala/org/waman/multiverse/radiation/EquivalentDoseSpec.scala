package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EquivalentDoseSpec
  extends AbstractQuantityAndUnitSpec[EquivalentDoseUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[EquivalentDoseUnit]

  "3.0 <<equivalent dose unit>> should be converted to the equivalent value in Sievert" in {
    __Exercise__
    val conversions =
      Table(
        ("equivalent doses", "expected"),
        (Seq(3.0.ySv, 3.0 ySv, 3.0 (ySv)), 3e-24),
        (Seq(3.0.zSv, 3.0 zSv, 3.0 (zSv)), 3e-21),
        (Seq(3.0.aSv, 3.0 aSv, 3.0 (aSv)), 3e-18),
        (Seq(3.0.fSv, 3.0 fSv, 3.0 (fSv)), 3e-15),
        (Seq(3.0.pSv, 3.0 pSv, 3.0 (pSv)), 3e-12),
        (Seq(3.0.nSv, 3.0 nSv, 3.0 (nSv)), 3e-9),
        (Seq(3.0.microSievert, 3.0 microSievert, 3.0 (microSievert)), 3e-6),
        (Seq(3.0.microSv, 3.0 microSv, 3.0 (microSv)), 3e-6),
        (Seq(3.0.μSv, 3.0 μSv, 3.0 (μSv)), 3e-6),
        (Seq(3.0.mSv, 3.0 mSv, 3.0 (mSv)), 3e-3),
        (Seq(3.0.cSv, 3.0 cSv, 3.0 (cSv)), 3e-2),
        (Seq(3.0.dSv, 3.0 dSv, 3.0 (dSv)), 3e-1),
        (Seq(3.0.Sv , 3.0 Sv , 3.0 (Sv)) , 3.0),
        (Seq(3.0.daSv, 3.0 daSv, 3.0 (daSv)), 3e1),
        (Seq(3.0.hSv, 3.0 hSv, 3.0 (hSv)), 3e2),
        (Seq(3.0.kSv, 3.0 kSv, 3.0 (kSv)), 3e3),
        (Seq(3.0.MSv, 3.0 MSv, 3.0 (MSv)), 3e6),
        (Seq(3.0.GSv, 3.0 GSv, 3.0 (GSv)), 3e9),
        (Seq(3.0.TSv, 3.0 TSv, 3.0 (TSv)), 3e12),
        (Seq(3.0.PSv, 3.0 PSv, 3.0 (PSv)), 3e15),
        (Seq(3.0.ESv, 3.0 ESv, 3.0 (ESv)), 3e18),
        (Seq(3.0.ZSv, 3.0 ZSv, 3.0 (ZSv)), 3e21),
        (Seq(3.0.YSv, 3.0 YSv, 3.0 (YSv)), 3e24),

        (Seq(3.0.yrem, 3.0 yrem, 3.0 (yrem)), 3e-26),
        (Seq(3.0.zrem, 3.0 zrem, 3.0 (zrem)), 3e-23),
        (Seq(3.0.arem, 3.0 arem, 3.0 (arem)), 3e-20),
        (Seq(3.0.frem, 3.0 frem, 3.0 (frem)), 3e-17),
        (Seq(3.0.prem, 3.0 prem, 3.0 (prem)), 3e-14),
        (Seq(3.0.nrem, 3.0 nrem, 3.0 (nrem)), 3e-11),
        (Seq(3.0.microRem, 3.0 microRem, 3.0 (microRem)), 3e-8),
        (Seq(3.0.μrem, 3.0 μrem, 3.0 (μrem)), 3e-8),
        (Seq(3.0.mrem, 3.0 mrem, 3.0 (mrem)), 3e-5),
        (Seq(3.0.crem, 3.0 crem, 3.0 (crem)), 3e-4),
        (Seq(3.0.drem, 3.0 drem, 3.0 (drem)), 3e-3),
        (Seq(3.0.rem , 3.0 rem , 3.0 (rem)) , 3e-2),
        (Seq(3.0.darem, 3.0 darem, 3.0 (darem)), 3e-1),
        (Seq(3.0.hrem, 3.0 hrem, 3.0 (hrem)), 3.0),
        (Seq(3.0.krem, 3.0 krem, 3.0 (krem)), 3e1),
        (Seq(3.0.Mrem, 3.0 Mrem, 3.0 (Mrem)), 3e4),
        (Seq(3.0.Grem, 3.0 Grem, 3.0 (Grem)), 3e7),
        (Seq(3.0.Trem, 3.0 Trem, 3.0 (Trem)), 3e10),
        (Seq(3.0.Prem, 3.0 Prem, 3.0 (Prem)), 3e13),
        (Seq(3.0.Erem, 3.0 Erem, 3.0 (Erem)), 3e16),
        (Seq(3.0.Zrem, 3.0 Zrem, 3.0 (Zrem)), 3e19),
        (Seq(3.0.Yrem, 3.0 Yrem, 3.0 (Yrem)), 3e22)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[EquivalentDose[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Sv) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Sv should be converted to the equivalent value in other equivalent dose units" in {
    __SetUp__
    val q = 3.0 (Sv)
    __Exercise__
    val conversions =
      Table(
        ("equivalent doses", "expected"),
        (Seq(q.ySv, q ySv, q (ySv)), 3e24),
        (Seq(q.zSv, q zSv, q (zSv)), 3e21),
        (Seq(q.aSv, q aSv, q (aSv)), 3e18),
        (Seq(q.fSv, q fSv, q (fSv)), 3e15),
        (Seq(q.pSv, q pSv, q (pSv)), 3e12),
        (Seq(q.nSv, q nSv, q (nSv)), 3e9),
        (Seq(q.μSv, q μSv, q (μSv)), 3e6),
        (Seq(q.mSv, q mSv, q (mSv)), 3e3),
        (Seq(q.cSv, q cSv, q (cSv)), 3e2),
        (Seq(q.dSv, q dSv, q (dSv)), 3e1),
        (Seq(q.Sv , q Sv , q (Sv)) , 3.0),
        (Seq(q.daSv, q daSv, q (daSv)), 3e-1),
        (Seq(q.hSv, q hSv, q (hSv)), 3e-2),
        (Seq(q.kSv, q kSv, q (kSv)), 3e-3),
        (Seq(q.MSv, q MSv, q (MSv)), 3e-6),
        (Seq(q.GSv, q GSv, q (GSv)), 3e-9),
        (Seq(q.TSv, q TSv, q (TSv)), 3e-12),
        (Seq(q.PSv, q PSv, q (PSv)), 3e-15),
        (Seq(q.ESv, q ESv, q (ESv)), 3e-18),
        (Seq(q.ZSv, q ZSv, q (ZSv)), 3e-21),
        (Seq(q.YSv, q YSv, q (YSv)), 3e-24),

        (Seq(q.yrem, q yrem, q (yrem)), 3e26),
        (Seq(q.zrem, q zrem, q (zrem)), 3e23),
        (Seq(q.arem, q arem, q (arem)), 3e20),
        (Seq(q.frem, q frem, q (frem)), 3e17),
        (Seq(q.prem, q prem, q (prem)), 3e14),
        (Seq(q.nrem, q nrem, q (nrem)), 3e11),
        (Seq(q.μrem, q μrem, q (μrem)), 3e8),
        (Seq(q.mrem, q mrem, q (mrem)), 3e5),
        (Seq(q.crem, q crem, q (crem)), 3e4),
        (Seq(q.drem, q drem, q (drem)), 3e3),
        (Seq(q.rem , q rem , q (rem)) , 3e2),
        (Seq(q.darem, q darem, q (darem)), 3e1),
        (Seq(q.hrem, q hrem, q (hrem)), 3.0),
        (Seq(q.krem, q krem, q (krem)), 3e-1),
        (Seq(q.Mrem, q Mrem, q (Mrem)), 3e-4),
        (Seq(q.Grem, q Grem, q (Grem)), 3e-7),
        (Seq(q.Trem, q Trem, q (Trem)), 3e-10),
        (Seq(q.Prem, q Prem, q (Prem)), 3e-13),
        (Seq(q.Erem, q Erem, q (Erem)), 3e-16),
        (Seq(q.Zrem, q Zrem, q (Zrem)), 3e-19),
        (Seq(q.Yrem, q Yrem, q (Yrem)), 3e-22)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
