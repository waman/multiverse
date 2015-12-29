package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

class TimeSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "Time object should be converted to proper value in millisecond" in {
    val conversions =
      Table(
        ("time"   , "expectedInMillisecond"),
        (Seq(1.0.ms    , 1.0 ms    , 1.0 (ms) ), 1.0),
        (Seq(1.0.s     , 1.0 s     , 1.0 (s)  ), 1000.0),
        (Seq(1.0.minute, 1.0 minute, 1.0 (min)), 60000.0),
        (Seq(1.0.h     , 1.0 h     , 1.0 (h)  ), 3600000.0)
      )

    forAll(conversions){ (ts: Seq[Time[Double]], expectedInMillisecond: Double) =>
      ts.foreach{ t =>
        (t ms) should equal (%(expectedInMillisecond))
      }
    }
  }

  "Time object should be converted to proper value in second" in {
    val conversions =
      Table(
        ("time"   , "expectedInSecond"),
        (Seq(1.0.ms    , 1.0 ms    , 1.0 (ms) ), 0.001),
        (Seq(1.0.s     , 1.0 s     , 1.0 (s)  ), 1.0),
        (Seq(1.0.minute, 1.0 minute, 1.0 (min)), 60.0),
        (Seq(1.0.h     , 1.0 h     , 1.0 (h)  ), 3600.0)
      )

    forAll(conversions){ (ts: Seq[Time[Double]], expectedInSecond: Double) =>
      ts.foreach{ t =>
        (t s) should equal (%(expectedInSecond))
      }
    }
  }

  "Time object should be converted to proper value in minute" in {
    val conversions =
      Table(
        ("time"   , "expectedInMinute"),
        (Seq(1.0.ms    , 1.0 ms    , 1.0 (ms) ), 1.0/60000.0),
        (Seq(1.0.s     , 1.0 s     , 1.0 (s)  ), 1.0/60.0),
        (Seq(1.0.minute, 1.0 minute, 1.0 (min)), 1.0),
        (Seq(1.0.h     , 1.0 h     , 1.0 (h)  ), 60.0)
      )

    forAll(conversions){ (ts: Seq[Time[Double]], expectedInMinute: Double) =>
      ts.foreach{ t =>
        (t min) should equal (%(expectedInMinute))
      }
    }
  }

  "Time object should be converted to proper value in hour" in {
    val conversions =
      Table(
        ("time"   , "expectedInHour"),
        (Seq(1.0.ms    , 1.0 ms    , 1.0 (ms) ), 1.0/3600000.0),
        (Seq(1.0.s     , 1.0 s     , 1.0 (s)  ), 1.0/3600.0),
        (Seq(1.0.minute, 1.0 minute, 1.0 (min)), 1.0/60.0),
        (Seq(1.0.h     , 1.0 h     , 1.0 (h)  ), 1.0)
      )

    forAll(conversions){ (ts: Seq[Time[Double]], expectedInHour: Double) =>
      ts.foreach{ t =>
        (t h) should equal (%(expectedInHour))
      }
    }
  }
}
