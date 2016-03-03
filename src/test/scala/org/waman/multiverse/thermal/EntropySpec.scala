package org.waman.multiverse.thermal

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EntropySpec
  extends AbstractQuantityAndUnitSpec[EntropyUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[EntropyUnit]

  val oneByte = 7.65595213e-23

  "Predefined entropy units" - {

    "3.0 <<entropy unit>> should be converted to the equivalent value in Joule per Kelvin" in {
      __Exercise__
      val conversions =
        Table(
          ("entropies", "expected"),
          (Seq(3.0.J/K , 3.0 J/K , 3.0 (J/K)) , 3.0),
          (Seq(3.0.nat , 3.0 nat , 3.0 (nat)) , 3.0 * 1.380650523e-23),
          (Seq(3.0.bit , 3.0 bit , 3.0 (bit)) , 3.0 * 9.56994016e-24),
          (Seq(3.0.Sh  , 3.0 Sh  , 3.0 (Sh))  , 3.0 * 9.56994016e-24),
          (Seq(3.0.ban , 3.0 ban , 3.0 (ban)) , 3.0 * 3.179065353e-23),
          (Seq(3.0.Hart, 3.0 Hart, 3.0 (Hart)), 3.0 * 3.179065353e-23),
          
          (Seq(3.0.B , 3.0 B , 3.0 (B)) , 3.0 * oneByte),
          (Seq(3.0.kB, 3.0 kB, 3.0 (kB)), 3e3 * oneByte),
          (Seq(3.0.MB, 3.0 MB, 3.0 (MB)), 3e6 * oneByte),
          (Seq(3.0.GB, 3.0 GB, 3.0 (GB)), 3e9 * oneByte),
          (Seq(3.0.TB, 3.0 TB, 3.0 (TB)), 3e12 * oneByte),
          (Seq(3.0.PB, 3.0 PB, 3.0 (PB)), 3e15 * oneByte),
          (Seq(3.0.EB, 3.0 EB, 3.0 (EB)), 3e18 * oneByte),
          (Seq(3.0.ZB, 3.0 ZB, 3.0 (ZB)), 3e21 * oneByte),
          (Seq(3.0.YB, 3.0 YB, 3.0 (YB)), 3e24 * oneByte),
          
          (Seq(3.0.KB , 3.0 KB , 3.0 (KB)) , 3.0 * 1024 * oneByte),
          (Seq(3.0.KiB, 3.0 KiB, 3.0 (KiB)), 3.0 * 1024 * oneByte),
          (Seq(3.0.MiB, 3.0 MiB, 3.0 (MiB)), 3.0 * Math.pow(1024, 2) * oneByte),
          (Seq(3.0.GiB, 3.0 GiB, 3.0 (GiB)), 3.0 * Math.pow(1024, 3) * oneByte),
          (Seq(3.0.TiB, 3.0 TiB, 3.0 (TiB)), 3.0 * Math.pow(1024, 4) * oneByte),
          (Seq(3.0.PiB, 3.0 PiB, 3.0 (PiB)), 3.0 * Math.pow(1024, 5) * oneByte),
          (Seq(3.0.EiB, 3.0 EiB, 3.0 (EiB)), 3.0 * Math.pow(1024, 6) * oneByte),
          (Seq(3.0.ZiB, 3.0 ZiB, 3.0 (ZiB)), 3.0 * Math.pow(1024, 7) * oneByte),
          (Seq(3.0.YiB, 3.0 YiB, 3.0 (YiB)), 3.0 * Math.pow(1024, 8) * oneByte)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Entropy[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut J/K) should equal(%%%(expected))
        }
      }
    }

    "3.0 J/K should be converted to the equivalent value in other entropy units" in {
      __SetUp__
      val q = 3.0 (J / K)
      __Exercise__
      val conversions =
        Table(
          ("entropies", "expected"),
          (Seq(q.J/K , q J/K , q (J/K)) , 3.0),
          (Seq(q.nat , q nat , q (nat)) , 3.0 / 1.380650523e-23),
          (Seq(q.bit , q bit , q (bit)) , 3.0 / 9.56994016e-24),
          (Seq(q.Sh  , q Sh  , q (Sh))  , 3.0 / 9.56994016e-24),
          (Seq(q.ban , q ban , q (ban)) , 3.0 / 3.179065353e-23),
          (Seq(q.Hart, q Hart, q (Hart)), 3.0 / 3.179065353e-23),

          (Seq(q.B , q B , q (B)) , 3.0 / oneByte),
          (Seq(q.kB, q kB, q (kB)), 3e-3 / oneByte),
          (Seq(q.MB, q MB, q (MB)), 3e-6 / oneByte),
          (Seq(q.GB, q GB, q (GB)), 3e-9 / oneByte),
          (Seq(q.TB, q TB, q (TB)), 3e-12 / oneByte),
          (Seq(q.PB, q PB, q (PB)), 3e-15 / oneByte),
          (Seq(q.EB, q EB, q (EB)), 3e-18 / oneByte),
          (Seq(q.ZB, q ZB, q (ZB)), 3e-21 / oneByte),
          (Seq(q.YB, q YB, q (YB)), 3e-24 / oneByte),

          (Seq(q.KB , q KB , q (KB)) , 3.0 / 1024 / oneByte),
          (Seq(q.KiB, q KiB, q (KiB)), 3.0 / 1024 / oneByte),
          (Seq(q.MiB, q MiB, q (MiB)), 3.0 / Math.pow(1024, 2) / oneByte),
          (Seq(q.GiB, q GiB, q (GiB)), 3.0 / Math.pow(1024, 3) / oneByte),
          (Seq(q.TiB, q TiB, q (TiB)), 3.0 / Math.pow(1024, 4) / oneByte),
          (Seq(q.PiB, q PiB, q (PiB)), 3.0 / Math.pow(1024, 5) / oneByte),
          (Seq(q.EiB, q EiB, q (EiB)), 3.0 / Math.pow(1024, 6) / oneByte),
          (Seq(q.ZiB, q ZiB, q (ZiB)), 3.0 / Math.pow(1024, 7) / oneByte),
          (Seq(q.YiB, q YiB, q (YiB)), 3.0 / Math.pow(1024, 8) / oneByte)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%(expected))
        }
      }
    }
  }

//  "Quotient entropy unit" - {
//
//    "Entropy unit of J/K should equal 1233.48183754752 m3" in {
//      __Exercise__
//      val sut = ac*ft
//      __Verify__
//      sut.unitInCubicMetre.toDouble should equal (%%%%(1233.48183754752))
//    }
//
//    "3.0 ac*ft should equal 3.0 * 43560.0 cu_ft" in {
//      __Exercise__
//      val conversions =
//        Table(
//          ("area", "expected"),
//          (3.0.ac*ft, 3.0 * 43560.0),
//          (3.0 ac*ft, 3.0 * 43560.0),
//          (3.0 (ac*ft), 3.0 * 43560.0)
//        )
//      __Verify__
//      forAll(conversions){ (sut: Volume[Double], expected: Double) =>
//        sut.cu_ft should equal (%%%%(expected))
//      }
//    }
//
//    "3.0 m3 should equal 3.0 / 1233.48183754752 ac*ft" in {
//      __SetUp__
//      val q = 3.0 (m3)
//      val expected = 3.0 / 1233.48183754752
//      __Exercise__
//      val conversions =
//        Table(
//          ("area", "expected"),
//          (q.ac*ft, expected),
//          (q ac*ft, expected),
//          (q (ac*ft), expected)
//        )
//      __Verify__
//      forAll(conversions){ (sut: Double, expected: Double) =>
//        sut should equal (%%%%(expected))
//      }
//    }
//  }
}
