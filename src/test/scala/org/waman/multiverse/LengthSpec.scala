package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

class LengthSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "Length object should be converted to proper value in millimetre" in {
    val conversions =
      Table(
        ("length"   , "expectedInMillimetre"),
        (Seq(1.0.mm, 1.0 mm, 1.0 (mm)), 1.0),
        (Seq(1.0.cm, 1.0 cm, 1.0 (cm)), 10.0),
        (Seq(1.0.m , 1.0 m , 1.0 (m)) , 1000.0),
        (Seq(1.0.km, 1.0 km, 1.0 (km)), 1000000.0)
      )

    forAll(conversions){ (ls: Seq[Length[Double]], expectedInMillimetre: Double) =>
      ls.foreach{ l =>
        (l mm) should equal (%(expectedInMillimetre))
      }
    }
  }

  "Length object should be converted to proper value in centimetre" in {
    val conversions =
      Table(
        ("length"   , "expectedInCentimetre"),
        (Seq(1.0.mm, 1.0 mm, 1.0 (mm)), 0.1),
        (Seq(1.0.cm, 1.0 cm, 1.0 (cm)), 1.0),
        (Seq(1.0.m , 1.0 m , 1.0 (m)) , 100.0),
        (Seq(1.0.km, 1.0 km, 1.0 (km)), 100000.0)
      )

    forAll(conversions){ (ls: Seq[Length[Double]], expectedInCentimetre: Double) =>
      ls.foreach{ l =>
        (l cm) should equal (%(expectedInCentimetre))
      }
    }
  }

  "Length object should be converted to proper value in metre" in {
    val conversions =
      Table(
        ("length"   , "expectedInMetre"),
        (Seq(1.0.mm, 1.0 mm, 1.0 (mm)), 0.001),
        (Seq(1.0.cm, 1.0 cm, 1.0 (cm)), 0.01),
        (Seq(1.0.m , 1.0 m , 1.0 (m)) , 1.0),
        (Seq(1.0.km, 1.0 km, 1.0 (km)), 1000.0),

        (Seq(1.0.in, 1.0 in, 1.0 (inch)), 0.0254),
        (Seq(1.0.ft  , 1.0 ft  , 1.0 (ft))  , 0.3048),
        (Seq(1.0.yd, 1.0 yd, 1.0 (yard)), 0.9144),
        (Seq(1.0.mi, 1.0 mi, 1.0 (mile)), 1609.344)
      )

    forAll(conversions){ (ls: Seq[Length[Double]], expectedInMetre: Double) =>
      ls.foreach{ l =>
        (l m) should equal (%(expectedInMetre))
      }
    }
  }

  "Length object should be converted to proper value in kilometre" in {
    val conversions =
      Table(
        ("length"   , "expectedInKilometre"),
        (Seq(1.0.mm, 1.0 mm, 1.0 (mm)), 0.000001),
        (Seq(1.0.cm, 1.0 cm, 1.0 (cm)), 0.00001),
        (Seq(1.0.m , 1.0 m , 1.0 (m)) , 0.001),
        (Seq(1.0.km, 1.0 km, 1.0 (km)), 1.0)
      )

    forAll(conversions){ (ls: Seq[Length[Double]], expectedInKilometre: Double) =>
      ls.foreach{ l =>
        (l km) should equal (%(expectedInKilometre))
      }
    }
  }

  "Conversion between inch, ft, yard and mile" in {
    ((1.0 ft) in) should equal (%(12.0))
    ((1.0 yd) ft) should equal (%(3.0))
    ((1.0 mi) yd) should equal (%(1760))
  }
}
