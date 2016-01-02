package org.waman.multiverse


import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

class VelocitySpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "Tests where converting from some units to m/s like 1.0 km/h => 1000.0/3600.0 m/s" in {
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

  val oneMpS = 3.0.`m/s`

  "Tests where converting metre unit to other units like 1.0 m => 1000.0 mm" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(oneMpS.`m/s`, oneMpS `m/s`, oneMpS (`m/s`), oneMpS.m/s, oneMpS m/s, oneMpS (m/s)), 3.0),
        (Seq(oneMpS.`km/h`, oneMpS `km/h`, oneMpS (`km/h`), oneMpS.km/h, oneMpS km/h, oneMpS (km/h)), 3.0*3600.0/1000.0),
        (Seq(oneMpS.km/min, oneMpS km/minute, oneMpS (km/min)), 3.0*60.0/1000.0),
        (Seq(oneMpS.cm/min, oneMpS cm/minute, oneMpS (cm/min)), 3.0*60.0/0.01)
      )

    forAll(conversions){ (vs: Seq[Double], expected: Double) =>
      vs.foreach{ v =>
        v should equal (%(expected))
      }
    }
  }
}
