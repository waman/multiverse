package org.waman.multiverse.time

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

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

  "Tests where converting second unit to other units like 1.0 s => 1000.0 ms" in {
    __SetUp__
    val value = 3.0 (s)
    __Exercise__
    val conversions =
      Table(
        ("times", "expected"),
        (Seq(value.ys, value ys, value (ys)), 3e24),
        (Seq(value.zs, value zs, value (zs)), 3e21),
        (Seq(value.as, value as, value (as)), 3e18),
        (Seq(value.fs, value fs, value (fs)), 3e15),
        (Seq(value.ps, value ps, value (ps)), 3e12),
        (Seq(value.ns, value ns, value (ns)), 3e9),
        (Seq(value.μs, value μs, value (μs)), 3e6),
        (Seq(value.ms, value ms, value (ms)), 3e3),
        (Seq(value.cs, value cs, value (cs)), 3e2),
        (Seq(value.ds, value ds, value (ds)), 3e1),
        (Seq(value.s , value s , value (s)) , 3.0),
        (Seq(value.das, value das, value (das)), 3e-1),
        (Seq(value.hs, value hs, value (hs)), 3e-2),
        (Seq(value.ks, value ks, value (ks)), 3e-3),
        (Seq(value.Ms, value Ms, value (Ms)), 3e-6),
        (Seq(value.Gs, value Gs, value (Gs)), 3e-9),
        (Seq(value.Ts, value Ts, value (Ts)), 3e-12),
        (Seq(value.Ps, value Ps, value (Ps)), 3e-15),
        (Seq(value.Es, value Es, value (Es)), 3e-18),
        (Seq(value.Zs, value Zs, value (Zs)), 3e-21),
        (Seq(value.Ys, value Ys, value (Ys)), 3e-24),

        (Seq(value.minute, value minute, value (minute)), 3.0/60.0),
        (Seq(value.h     , value h     , value (h))  , 3.0/(60.0 * 60.0)),
        (Seq(value.d     , value d     , value (d))  , 3.0/(60.0 * 60.0 * 24.0))
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
