package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TimeSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of time" in {
    __SetUp__
    import TimeUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[TimeUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      YoctoSecond,
      ZeptoSecond,
      AttoSecond,
      FemtoSecond,
      PicoSecond,
      NanoSecond,
      MicroSecond,
      MilliSecond,
      CentiSecond,
      DeciSecond,
      Second,
      DecaSecond,
      HectoSecond,
      KiloSecond,
      MegaSecond,
      GigaSecond,
      TeraSecond,
      PetaSecond,
      ExaSecond,
      ZettaSecond,
      YottaSecond,

      Minute,
      Hour,
      Day
    )
  }

  "Tests where converting from some units to second like 1.0 ms => 0.001 s" in {
    val conversions =
      Table(
        ("time", "expected"),
        (Seq(3.0.ys, 3.0 ys, 3.0 (ys)), 3e-24),
        (Seq(3.0.zs, 3.0 zs, 3.0 (zs)), 3e-21),
        (Seq(3.0.as, 3.0 as, 3.0 (as)), 3e-18),
        (Seq(3.0.fs, 3.0 fs, 3.0 (fs)), 3e-15),
        (Seq(3.0.ps, 3.0 ps, 3.0 (ps)), 3e-12),
        (Seq(3.0.ns, 3.0 ns, 3.0 (ns)), 3e-9),
        (Seq(3.0.μs, 3.0 μs, 3.0 (μs)), 3e-6),
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

        (Seq(3.0.minute, 3.0 minute, 3.0 (min)), 3.0 * 60.0),
        (Seq(3.0.h     , 3.0 h     , 3.0 (h))  , 3.0 * 60.0 * 60.0),
        (Seq(3.0.d     , 3.0 d     , 3.0 (d))  , 3.0 * 60.0 * 60.0 * 24.0)
      )

    forAll(conversions){ (ts: Seq[Time[Double]], expected: Double) =>
      ts.foreach{ t =>
        (t s) should equal (%(expected))
      }
    }
  }

  val threeSecond = 3.0 s

  "Tests where converting second unit to other units like 1.0 s => 1000.0 ms" in {
    val conversions =
      Table(
        ("time", "expected"),
        (Seq(threeSecond.ys, threeSecond ys, threeSecond (ys)), 3e24),
        (Seq(threeSecond.zs, threeSecond zs, threeSecond (zs)), 3e21),
        (Seq(threeSecond.as, threeSecond as, threeSecond (as)), 3e18),
        (Seq(threeSecond.fs, threeSecond fs, threeSecond (fs)), 3e15),
        (Seq(threeSecond.ps, threeSecond ps, threeSecond (ps)), 3e12),
        (Seq(threeSecond.ns, threeSecond ns, threeSecond (ns)), 3e9),
        (Seq(threeSecond.μs, threeSecond μs, threeSecond (μs)), 3e6),
        (Seq(threeSecond.ms, threeSecond ms, threeSecond (ms)), 3e3),
        (Seq(threeSecond.cs, threeSecond cs, threeSecond (cs)), 3e2),
        (Seq(threeSecond.ds, threeSecond ds, threeSecond (ds)), 3e1),
        (Seq(threeSecond.s , threeSecond s , threeSecond (s)) , 3.0),
        (Seq(threeSecond.das, threeSecond das, threeSecond (das)), 3e-1),
        (Seq(threeSecond.hs, threeSecond hs, threeSecond (hs)), 3e-2),
        (Seq(threeSecond.ks, threeSecond ks, threeSecond (ks)), 3e-3),
        (Seq(threeSecond.Ms, threeSecond Ms, threeSecond (Ms)), 3e-6),
        (Seq(threeSecond.Gs, threeSecond Gs, threeSecond (Gs)), 3e-9),
        (Seq(threeSecond.Ts, threeSecond Ts, threeSecond (Ts)), 3e-12),
        (Seq(threeSecond.Ps, threeSecond Ps, threeSecond (Ps)), 3e-15),
        (Seq(threeSecond.Es, threeSecond Es, threeSecond (Es)), 3e-18),
        (Seq(threeSecond.Zs, threeSecond Zs, threeSecond (Zs)), 3e-21),
        (Seq(threeSecond.Ys, threeSecond Ys, threeSecond (Ys)), 3e-24),

        (Seq(threeSecond.minute, threeSecond minute, threeSecond (min)), 3.0/60.0),
        (Seq(threeSecond.h     , threeSecond h     , threeSecond (h))  , 3.0/(60.0 * 60.0)),
        (Seq(threeSecond.d     , threeSecond d     , threeSecond (d))  , 3.0/(60.0 * 60.0 * 24.0))
      )

    forAll(conversions){ (ts: Seq[Double], expected: Double) =>
      ts.foreach{ t =>
        t should equal (%(expected))
      }
    }
  }
}
