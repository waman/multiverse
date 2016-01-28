package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

class TimeSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "UnitSystem#getSupportedUnits method should return supported units of time" in {
    __SetUp__
    import TimeUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[TimeUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      Nanosecond,
      Microsecond,
      Millisecond,
      Second,
      Minute,
      Hour,
      Day
    )
  }

  "Tests where converting from some units to second like 1.0 ms => 0.001 s" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(1.0.ns    , 1.0 ns    , 1.0 (ns)) , 1e-9),
        (Seq(1.0.μs    , 1.0 μs    , 1.0 (μs)) , 1e-6),
        (Seq(1.0.ms    , 1.0 ms    , 1.0 (ms)) , 1e-3),
        (Seq(1.0.s     , 1.0 s     , 1.0 (s))  , 1.0),
        (Seq(1.0.minute, 1.0 minute, 1.0 (min)), 60.0),
        (Seq(1.0.h     , 1.0 h     , 1.0 (h))  , 3600.0),
        (Seq(1.0.d     , 1.0 d     , 1.0 (d))  , 3600.0 * 24.0)
      )

    forAll(conversions){ (ts: Seq[Time[Double]], expected: Double) =>
      ts.foreach{ t =>
        (t s) should equal (%(expected))
      }
    }
  }

  val oneSecond = 1.0 s

  "Tests where converting second unit to other units like 1.0 s => 1000.0 ms" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(oneSecond.ns    , oneSecond ns    , oneSecond (ns)) , 1e9),
        (Seq(oneSecond.μs    , oneSecond μs    , oneSecond (μs)) , 1e6),
        (Seq(oneSecond.ms    , oneSecond ms    , oneSecond (ms)) , 1e3),
        (Seq(oneSecond.s     , oneSecond s     , oneSecond (s))  , 1.0),
        (Seq(oneSecond.minute, oneSecond minute, oneSecond (min)), 1.0/60.0),
        (Seq(oneSecond.h     , oneSecond h     , oneSecond (h))  , 1.0/3600.0),
        (Seq(oneSecond.d     , oneSecond d     , oneSecond (d))  , 1.0/(3600.0 * 24.0))
      )

    forAll(conversions){ (ts: Seq[Double], expected: Double) =>
      ts.foreach{ t =>
        t should equal (%(expected))
      }
    }
  }
}
