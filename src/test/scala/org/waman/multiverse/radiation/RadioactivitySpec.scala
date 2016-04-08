package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class RadioactivitySpec
  extends AbstractQuantityAndUnitSpec[RadioactivityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[RadioactivityUnit]

  "3.0 <<radioactivity unit>> should be converted to the equivalent value in Becquerel" in {
    __Exercise__
    val conversions =
      Table(
        ("radioactivities", "expected"),
        (Seq(3.0.yBq, 3.0 yBq, 3.0 (yBq)), 3e-24),
        (Seq(3.0.zBq, 3.0 zBq, 3.0 (zBq)), 3e-21),
        (Seq(3.0.aBq, 3.0 aBq, 3.0 (aBq)), 3e-18),
        (Seq(3.0.fBq, 3.0 fBq, 3.0 (fBq)), 3e-15),
        (Seq(3.0.pBq, 3.0 pBq, 3.0 (pBq)), 3e-12),
        (Seq(3.0.nBq, 3.0 nBq, 3.0 (nBq)), 3e-9),
        (Seq(3.0.μBq, 3.0 μBq, 3.0 (μBq)), 3e-6),
        (Seq(3.0.mcBq, 3.0 mcBq, 3.0 (mcBq)), 3e-6),
        (Seq(3.0.mBq, 3.0 mBq, 3.0 (mBq)), 3e-3),
        (Seq(3.0.cBq, 3.0 cBq, 3.0 (cBq)), 3e-2),
        (Seq(3.0.dBq, 3.0 dBq, 3.0 (dBq)), 3e-1),
        (Seq(3.0.Bq , 3.0 Bq , 3.0 (Bq)) , 3.0),
        (Seq(3.0.daBq, 3.0 daBq, 3.0 (daBq)), 3e1),
        (Seq(3.0.hBq, 3.0 hBq, 3.0 (hBq)), 3e2),
        (Seq(3.0.kBq, 3.0 kBq, 3.0 (kBq)), 3e3),
        (Seq(3.0.MBq, 3.0 MBq, 3.0 (MBq)), 3e6),
        (Seq(3.0.GBq, 3.0 GBq, 3.0 (GBq)), 3e9),
        (Seq(3.0.TBq, 3.0 TBq, 3.0 (TBq)), 3e12),
        (Seq(3.0.PBq, 3.0 PBq, 3.0 (PBq)), 3e15),
        (Seq(3.0.EBq, 3.0 EBq, 3.0 (EBq)), 3e18),
        (Seq(3.0.ZBq, 3.0 ZBq, 3.0 (ZBq)), 3e21),
        (Seq(3.0.YBq, 3.0 YBq, 3.0 (YBq)), 3e24),

        (Seq(3.0.yCi, 3.0 yCi, 3.0 (yCi)), 3.0 * 3.7e-14),
        (Seq(3.0.zCi, 3.0 zCi, 3.0 (zCi)), 3.0 * 3.7e-11),
        (Seq(3.0.aCi, 3.0 aCi, 3.0 (aCi)), 3.0 * 3.7e-8),
        (Seq(3.0.fCi, 3.0 fCi, 3.0 (fCi)), 3.0 * 3.7e-5),
        (Seq(3.0.pCi, 3.0 pCi, 3.0 (pCi)), 3.0 * 3.7e-2),
        (Seq(3.0.nCi, 3.0 nCi, 3.0 (nCi)), 3.0 * 3.7e1),
        (Seq(3.0.μCi, 3.0 μCi, 3.0 (μCi)), 3.0 * 3.7e4),
        (Seq(3.0.mcCi, 3.0 mcCi, 3.0 (mcCi)), 3.0 * 3.7e4),
        (Seq(3.0.mCi, 3.0 mCi, 3.0 (mCi)), 3.0 * 3.7e7),
        (Seq(3.0.cCi, 3.0 cCi, 3.0 (cCi)), 3.0 * 3.7e8),
        (Seq(3.0.dCi, 3.0 dCi, 3.0 (dCi)), 3.0 * 3.7e9),
        (Seq(3.0.Ci , 3.0 Ci , 3.0 (Ci)) , 3.0 * 3.7e10),
        (Seq(3.0.daCi, 3.0 daCi, 3.0 (daCi)), 3.0 * 3.7e11),
        (Seq(3.0.hCi, 3.0 hCi, 3.0 (hCi)), 3.0 * 3.7e12),
        (Seq(3.0.kCi, 3.0 kCi, 3.0 (kCi)), 3.0 * 3.7e13),
        (Seq(3.0.MCi, 3.0 MCi, 3.0 (MCi)), 3.0 * 3.7e16),
        (Seq(3.0.GCi, 3.0 GCi, 3.0 (GCi)), 3.0 * 3.7e19),
        (Seq(3.0.TCi, 3.0 TCi, 3.0 (TCi)), 3.0 * 3.7e22),
        (Seq(3.0.PCi, 3.0 PCi, 3.0 (PCi)), 3.0 * 3.7e25),
        (Seq(3.0.ECi, 3.0 ECi, 3.0 (ECi)), 3.0 * 3.7e28),
        (Seq(3.0.ZCi, 3.0 ZCi, 3.0 (ZCi)), 3.0 * 3.7e31),
        (Seq(3.0.YCi, 3.0 YCi, 3.0 (YCi)), 3.0 * 3.7e34),

        (Seq(3.0.Rd, 3.0 Rd, 3.0 (Rd)), 3e6)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Radioactivity[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Bq) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Bq should be converted to the equivalent value in other radioactivity units" in {
    __SetUp__
    val q = 3.0 (Bq)
    __Exercise__
    val conversions =
      Table(
        ("radioactivities", "expected"),
        (Seq(q.yBq, q yBq, q (yBq)), 3e24),
        (Seq(q.zBq, q zBq, q (zBq)), 3e21),
        (Seq(q.aBq, q aBq, q (aBq)), 3e18),
        (Seq(q.fBq, q fBq, q (fBq)), 3e15),
        (Seq(q.pBq, q pBq, q (pBq)), 3e12),
        (Seq(q.nBq, q nBq, q (nBq)), 3e9),
        (Seq(q.μBq, q μBq, q (μBq)), 3e6),
        (Seq(q.mcBq, q mcBq, q (mcBq)), 3e6),
        (Seq(q.mBq, q mBq, q (mBq)), 3e3),
        (Seq(q.cBq, q cBq, q (cBq)), 3e2),
        (Seq(q.dBq, q dBq, q (dBq)), 3e1),
        (Seq(q.Bq , q Bq , q (Bq)) , 3.0),
        (Seq(q.daBq, q daBq, q (daBq)), 3e-1),
        (Seq(q.hBq, q hBq, q (hBq)), 3e-2),
        (Seq(q.kBq, q kBq, q (kBq)), 3e-3),
        (Seq(q.MBq, q MBq, q (MBq)), 3e-6),
        (Seq(q.GBq, q GBq, q (GBq)), 3e-9),
        (Seq(q.TBq, q TBq, q (TBq)), 3e-12),
        (Seq(q.PBq, q PBq, q (PBq)), 3e-15),
        (Seq(q.EBq, q EBq, q (EBq)), 3e-18),
        (Seq(q.ZBq, q ZBq, q (ZBq)), 3e-21),
        (Seq(q.YBq, q YBq, q (YBq)), 3e-24),

        (Seq(q.yCi, q yCi, q (yCi)), 3.0 / 3.7e-14),
        (Seq(q.zCi, q zCi, q (zCi)), 3.0 / 3.7e-11),
        (Seq(q.aCi, q aCi, q (aCi)), 3.0 / 3.7e-8),
        (Seq(q.fCi, q fCi, q (fCi)), 3.0 / 3.7e-5),
        (Seq(q.pCi, q pCi, q (pCi)), 3.0 / 3.7e-2),
        (Seq(q.nCi, q nCi, q (nCi)), 3.0 / 3.7e1),
        (Seq(q.μCi, q μCi, q (μCi)), 3.0 / 3.7e4),
        (Seq(q.mcCi, q mcCi, q (mcCi)), 3.0 / 3.7e4),
        (Seq(q.mCi, q mCi, q (mCi)), 3.0 / 3.7e7),
        (Seq(q.cCi, q cCi, q (cCi)), 3.0 / 3.7e8),
        (Seq(q.dCi, q dCi, q (dCi)), 3.0 / 3.7e9),
        (Seq(q.Ci , q Ci , q (Ci)) , 3.0 / 3.7e10),
        (Seq(q.daCi, q daCi, q (daCi)), 3.0 / 3.7e11),
        (Seq(q.hCi, q hCi, q (hCi)), 3.0 / 3.7e12),
        (Seq(q.kCi, q kCi, q (kCi)), 3.0 / 3.7e13),
        (Seq(q.MCi, q MCi, q (MCi)), 3.0 / 3.7e16),
        (Seq(q.GCi, q GCi, q (GCi)), 3.0 / 3.7e19),
        (Seq(q.TCi, q TCi, q (TCi)), 3.0 / 3.7e22),
        (Seq(q.PCi, q PCi, q (PCi)), 3.0 / 3.7e25),
        (Seq(q.ECi, q ECi, q (ECi)), 3.0 / 3.7e28),
        (Seq(q.ZCi, q ZCi, q (ZCi)), 3.0 / 3.7e31),
        (Seq(q.YCi, q YCi, q (YCi)), 3.0 / 3.7e34),

        (Seq(q.Rd, q Rd, q (Rd)), 3e-6)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
