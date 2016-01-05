package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps
import spire.implicits._

class LengthSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "Tests where converting from some units to metre like 3.0 mm => 0.003 m" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(3.0.nm, 3.0 nm, 3.0 (nm)), 3e-9),
        (Seq(3.0.µm, 3.0 µm, 3.0 (µm)), 3e-6),
        (Seq(3.0.mm, 3.0 mm, 3.0 (mm)), 3e-3),
        (Seq(3.0.cm, 3.0 cm, 3.0 (cm)), 3e-2),
        (Seq(3.0.m , 3.0 m , 3.0 (m)) , 3.0),
        (Seq(3.0.km, 3.0 km, 3.0 (km)), 3e3),
        (Seq(3.0.Mm, 3.0 Mm, 3.0 (Mm)), 3e6),
        (Seq(3.0.Gm, 3.0 Gm, 3.0 (Gm)), 3e9),
        (Seq(3.0.Tm, 3.0 Tm, 3.0 (Tm)), 3e12),

        // astronomy
        (Seq(3.0.au, 3.0 au, 3.0 (au)), 3.0 * 149597870700.0),
        (Seq(3.0.ly, 3.0 ly, 3.0 (ly)), 3.0 * 9.4607304725808e15),
        (Seq(3.0.pc, 3.0 pc, 3.0 (pc)), 3.0 * 3.08567782e16),

        // yard-pond
        (Seq(3.0.in, 3.0 in, 3.0 (in)), 3.0 * 0.0254),
        (Seq(3.0.ft, 3.0 ft, 3.0 (ft)), 3.0 * 0.3048),
        (Seq(3.0.yd, 3.0 yd, 3.0 (yd)), 3.0 * 0.9144),
        (Seq(3.0.mi, 3.0 mi, 3.0 (mi)), 3.0 * 1609.344)
      )

    forAll(conversions){ (ls: Seq[Length[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l m) should equal (%(expected))
      }
    }
  }

  val threeMetre = 3.0 m

  "Tests where converting metre unit to other units like 3.0 m => 3000.0 mm" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(threeMetre.nm, threeMetre nm, threeMetre (nm)), 3e9),
        (Seq(threeMetre.µm, threeMetre µm, threeMetre (µm)), 3e6),
        (Seq(threeMetre.mm, threeMetre mm, threeMetre (mm)), 3e3),
        (Seq(threeMetre.cm, threeMetre cm, threeMetre (cm)), 3e2),
        (Seq(threeMetre.m , threeMetre m , threeMetre (m)) , 3.0),
        (Seq(threeMetre.km, threeMetre km, threeMetre (km)), 3e-3),
        (Seq(threeMetre.Mm, threeMetre Mm, threeMetre (Mm)), 3e-6),
        (Seq(threeMetre.Gm, threeMetre Gm, threeMetre (Gm)), 3e-9),
        (Seq(threeMetre.Tm, threeMetre Tm, threeMetre (Tm)), 3e-12),

        // astronomy
        (Seq(threeMetre.au, threeMetre au, threeMetre (au)), 3.0/149597870700.0),
        (Seq(threeMetre.ly, threeMetre ly, threeMetre (ly)), 3.0/9.4607304725808e15),
        (Seq(threeMetre.pc, threeMetre pc, threeMetre (pc)), 3.0/3.08567782e16),

        // yard-pond
        (Seq(threeMetre.in, threeMetre in, threeMetre (in)), 3.0/0.0254),
        (Seq(threeMetre.ft, threeMetre ft, threeMetre (ft)), 3.0/0.3048),
        (Seq(threeMetre.yd, threeMetre yd, threeMetre (yd)), 3.0/0.9144),
        (Seq(threeMetre.mi, threeMetre mi, threeMetre (mi)), 3.0/1609.344)
      )

    forAll(conversions){ (ls: Seq[Double], expected: Double) =>
      ls.foreach{ l =>
        l should equal (%(expected))
      }
    }
  }

  "Conversion between inch, ft, yard and mile" in {
    ((3.0 ft) in) should equal (%(3.0 * 12.0))
    ((3.0 yd) ft) should equal (%(3.0 * 3.0))
    ((3.0 mi) yd) should equal (%(3.0 * 1760))
  }
}
