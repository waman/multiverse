package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

class VelocitySpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "UnitSystem#getSupportedUnits method should return supported units of velocity" in {
    __SetUp__
    import VelocityUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[VelocityUnit])
    __Verify__
    result should contain allOf (
      MetrePerSecond,
      KilometrePerHour)
  }

  "VelocityUnit should" - {

    "return a velocity value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
      __SetUp__
      val vu = km/h
      __Verify__
      vu.inMetrePerSecond should equal (r"1000" / r"3600")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (km/h) should equal (km/h)
      (km/h).hashCode should equal ((km/h).hashCode)
    }
  }

  "Tests where converting from some units to m/s like 3.0 km/h => 3.0 * 1000.0/3600.0 m/s" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(3.0.`m/s` , 3.0 `m/s` , 3.0 (`m/s`) , 3.0.m/s , 3.0 m/s , 3.0 (m/s)) , 3.0),
        (Seq(3.0.`km/h`, 3.0 `km/h`, 3.0 (`km/h`), 3.0.km/h, 3.0 km/h, 3.0 (km/h)), 3.0*1000.0/3600.0),
        (Seq(3.0.km/min, 3.0 km/minute, 3.0 (km/min)), 3.0*1000.0/60.0),
        (Seq(3.0.cm/min, 3.0 cm/minute, 3.0 (cm/min)), 3.0*0.01/60.0)
      )

    forAll(conversions){ (vs: Seq[Velocity[Double]], expected: Double) =>
      vs.foreach{ v =>
        (v `m/s`) should equal (%(expected))
      }
    }
  }

  val threeMps = 3.0.`m/s`

  "Tests where converting metre unit to other units like 3.0 m => 3000.0 mm" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(threeMps.`m/s`, threeMps `m/s`, threeMps (`m/s`), threeMps.m/s, threeMps m/s, threeMps (m/s)), 3.0),
        (Seq(threeMps.`km/h`, threeMps `km/h`, threeMps (`km/h`), threeMps.km/h, threeMps km/h, threeMps (km/h)), 3.0*3600.0/1000.0),
        (Seq(threeMps.km/min, threeMps km/minute, threeMps (km/min)), 3.0*60.0/1000.0),
        (Seq(threeMps.cm/min, threeMps cm/minute, threeMps (cm/min)), 3.0*60.0/0.01)
      )

    forAll(conversions){ (vs: Seq[Double], expected: Double) =>
      vs.foreach{ v =>
        v should equal (%(expected))
      }
    }
  }
}
