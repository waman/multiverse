package waman.multiverse

import spire.math.Real
import waman.multiverse.unit.BasicUnits._
import waman.multiverse.unit.MechanicalUnits._

class TypelessLinearUnitSpec extends MultiverseCustomSpec{

  "Unit" - {

    "Types" - {

      "Sample units should have the proper type" in {
        (cm/m) shouldBe a [TypelessQuotientUnit]
        (cm*min) shouldBe a [TypelessProductUnit]
        (km/L) shouldBe a [TypelessQuotientUnit]
      }

      "m/m should be the Dimless" in {
        (m/m) should be (Dimless)
      }
    }

    "Properties" - {

      "the name property should have the proper value" in {
        val conversions =
          Table(
            ("unit", "expected"),
            (Dimless, "dimensionless unit"),  // Dimless
            (cm/m, "centimetre per metre"),

            // product
            (cm*min, "centimetre times minute"),
            ((cm*min)*L, "centimetre times minute times litre"),
            ((cm*min)/L, "centimetre times minute per litre"),
            ((cm*min)/cm, "minute"),
            ((cm*min)/min, "centimetre"),

            // quotient
            (km/L, "kilometre per litre"),
            ((km/L)*L, "kilometre"),
            ((km/L)*cm3, "kilometre times cubic centimetre per litre"),  // km*cm³/L
            ((km/L)/m, "kilometre per litre times metre"),  // km/(L*m)
            ((km/L)/km, "one per litre")  // 1/L
          )

        forAll(conversions){ (unit: TypelessLinearUnit, expected: String) =>
          // Exercise
          val sut = unit.name
          //Verify
          sut should equal (expected)
        }
      }

      "the symbol property should have the proper value" in {
        val conversions =
          Table(
            ("unit", "expected"),
            (Dimless, "[null]"),  // Dimless
            (cm/m, "cm/m"),

            // product
            (cm*min, "cm*min"),
            ((cm*min)*L, "cm*min*L"),
            ((cm*min)/L, "cm*min/L"),
            ((cm*min)/cm, "min"),
            ((cm*min)/min, "cm"),

            // quotient
            (km/L, "km/L"),
            ((km/L)*L, "km"),
            ((km/L)*cm3, "km*cm³/L"),
            ((km/L)/m, "km/(L*m)"),
            ((km/L)/km, "1/L"),

            // more complex
            ((cm*min)*min/min, "cm*min"),
            ((cm*min)*min/cm, "min*min"),
            ((km/L)*(L/s), "km/s"),
            ((km/L)*(kg/km), "kg/L"),
            ((km/L)/(s/L), "km/s"),
            ((km/L)/(km/kg), "kg/L")
          )

        forAll(conversions){ (unit: TypelessLinearUnit, expected: String) =>
          // Exercise
          val sut = unit.symbol
          //Verify
          sut should equal (expected)
        }
      }

      "the getSIUnit method should have the proper unit instance" in {
        val conversions =
          Table(
            ("unit", "expected"),
            (Dimless, Dimless),  // Dimless
            (cm/m, Dimless),

            // product
            (cm*min, m*s),
            ((cm*min)*L, m*s*m3),
            ((cm*min)/L, m*s/m3),
            ((cm*min)/cm, s),
            ((cm*min)/min, m),

            // quotient
            (km/L, m/m3),
            ((km/L)*L, m),
            ((km/L)*cm3, m),
            ((km/L)/m, Dimless/m3),
            ((km/L)/km, Dimless/m3)
          )

        forAll(conversions){ (unit: TypelessLinearUnit, expected: LinearUnit[_]) =>
          // Exercise
          val sut = unit.getSIUnit
          //Verify
          sut should equal (expected)
        }
      }

      "the interval method should have the proper value" in {
        val conversions =
          Table(
            ("unit", "expected"),
            (Dimless, Real.one),  // Dimless
            (cm/m, Real("0.01")),

            // product
            (cm*min, Real("0.6")),
            ((cm*min)*L, Real("0.0006")),
            ((cm*min)/L, Real(600)),
            ((cm*min)/cm, Real(60)),
            ((cm*min)/min, Real("0.01")),

            // quotient
            (km/L, Real("1e6")),
            ((km/L)*L, Real("1000")),
            ((km/L)*cm3, Real.one),
            ((km/L)/m, Real("1e6")),
            ((km/L)/km, Real("1000"))
          )

        forAll(conversions){ (unit: TypelessLinearUnit, expected: Real) =>
          // Exercise
          val sut = unit.interval
          //Verify
          sut should equal (expected)
        }
      }
    }

    "Equivalence" - {

      "TypelessProductUnit should be equivalent to the ordinal product unit" in {
        // SetUp
        val v = N*m
        // Exercise
        val sut = TypelessLinearUnit(v)
        // Verify
        sut shouldBe a [TypelessProductUnit]
        sut.isEquivalentTo(v) should be (true)
      }
    }

    "TypelessQuotientUnit should be equivalent to the ordinal quotient unit" in {
      // SetUp
      val v = m/s
      // Exercise
      val sut = TypelessLinearUnit(v)
      // Verify
      sut shouldBe a [TypelessQuotientUnit]
      sut.isEquivalentTo(v) should be (true)
    }
  }
}
