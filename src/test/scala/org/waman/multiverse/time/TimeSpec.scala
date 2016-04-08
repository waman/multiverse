package org.waman.multiverse.time

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TimeSpec
  extends AbstractQuantityAndUnitSpec[TimeUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[TimeUnit]

  "3.0 <<time unit>> should be converted to the equivalent value in second" in {
    __Exercise__
    val conversions =
      Table(
        ("times", "expected"),
        (Seq(3.0.ys, 3.0 ys, 3.0 (ys)), 3e-24),
        (Seq(3.0.zs, 3.0 zs, 3.0 (zs)), 3e-21),
        (Seq(3.0.as, 3.0 as, 3.0 (as)), 3e-18),
        (Seq(3.0.fs, 3.0 fs, 3.0 (fs)), 3e-15),
        (Seq(3.0.ps, 3.0 ps, 3.0 (ps)), 3e-12),
        (Seq(3.0.ns, 3.0 ns, 3.0 (ns)), 3e-9),
        (Seq(3.0.μs, 3.0 μs, 3.0 (μs)), 3e-6),
        (Seq(3.0.mcs, 3.0 mcs, 3.0 (mcs)), 3e-6),
        (Seq(3.0.ms, 3.0 ms, 3.0 (ms)), 3e-3),
        (Seq(3.0.cs, 3.0 cs, 3.0 (cs)), 3e-2),
        (Seq(3.0.ds, 3.0 ds, 3.0 (ds)), 3e-1),
        (Seq(3.0.s , 3.0 s , 3.0 (s))  , 3.0),
        (Seq(3.0.das, 3.0 das, 3.0 (das)), 3e1),
        (Seq(3.0.hs, 3.0 hs, 3.0 (hs)), 3e2),
        (Seq(3.0.ks, 3.0 ks, 3.0 (ks)), 3e3),
        (Seq(3.0.Ms, 3.0 Ms, 3.0 (Ms)), 3e6),
        (Seq(3.0.Gs, 3.0 Gs, 3.0 (Gs)), 3e9),
        (Seq(3.0.Ts, 3.0 Ts, 3.0 (Ts)), 3e12),
        (Seq(3.0.Ps, 3.0 Ps, 3.0 (Ps)), 3e15),
        (Seq(3.0.Es, 3.0 Es, 3.0 (Es)), 3e18),
        (Seq(3.0.Zs, 3.0 Zs, 3.0 (Zs)), 3e21),
        (Seq(3.0.Ys, 3.0 Ys, 3.0 (Ys)), 3e24),

        (Seq(3.0.minute, 3.0 minute, 3.0 (minute)), 3.0 * 60.0),
        (Seq(3.0.h     , 3.0 h     , 3.0 (h))  , 3.0 * 60.0 * 60.0),
        (Seq(3.0.d     , 3.0 d     , 3.0 (d))  , 3.0 * 60.0 * 60.0 * 24.0)
      )

    forAll(conversions){ (suts: Seq[Time[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut s) should equal (%%%%(expected))
      }
    }
  }

  "3.0 s should be converted to the equivalent value in other time units" in {
    __SetUp__
    val q = 3.0 (s)
    __Exercise__
    val conversions =
      Table(
        ("times", "expected"),
        (Seq(q.ys, q ys, q (ys)), 3e24),
        (Seq(q.zs, q zs, q (zs)), 3e21),
        (Seq(q.as, q as, q (as)), 3e18),
        (Seq(q.fs, q fs, q (fs)), 3e15),
        (Seq(q.ps, q ps, q (ps)), 3e12),
        (Seq(q.ns, q ns, q (ns)), 3e9),
        (Seq(q.μs, q μs, q (μs)), 3e6),
        (Seq(q.mcs, q mcs, q (mcs)), 3e6),
        (Seq(q.ms, q ms, q (ms)), 3e3),
        (Seq(q.cs, q cs, q (cs)), 3e2),
        (Seq(q.ds, q ds, q (ds)), 3e1),
        (Seq(q.s , q s , q (s)) , 3.0),
        (Seq(q.das, q das, q (das)), 3e-1),
        (Seq(q.hs, q hs, q (hs)), 3e-2),
        (Seq(q.ks, q ks, q (ks)), 3e-3),
        (Seq(q.Ms, q Ms, q (Ms)), 3e-6),
        (Seq(q.Gs, q Gs, q (Gs)), 3e-9),
        (Seq(q.Ts, q Ts, q (Ts)), 3e-12),
        (Seq(q.Ps, q Ps, q (Ps)), 3e-15),
        (Seq(q.Es, q Es, q (Es)), 3e-18),
        (Seq(q.Zs, q Zs, q (Zs)), 3e-21),
        (Seq(q.Ys, q Ys, q (Ys)), 3e-24),

        (Seq(q.minute, q minute, q (minute)), 3.0/60.0),
        (Seq(q.h     , q h     , q (h))  , 3.0/(60.0 * 60.0)),
        (Seq(q.d     , q d     , q (d))  , 3.0/(60.0 * 60.0 * 24.0))
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
