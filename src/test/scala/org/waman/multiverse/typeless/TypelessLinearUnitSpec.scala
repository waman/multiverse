package org.waman.multiverse.typeless

import spire.math.Real
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.custom.BasicUnits._
import org.waman.multiverse.unit.custom.MechanicalUnits._
import org.waman.multiverse.{LinearUnit, MultiverseCustomSpec}

class TypelessLinearUnitSpec extends MultiverseCustomSpec{

  "Unit" - {

    "Types" - {

      "Sample units should have the proper type" in {
        import org.waman.multiverse.typeless.implicits._
        import org.waman.multiverse.unit.custom.ElectromagneticUnits.C

        // Dimless
        (m/m) should be (Dimless)
        m^0 should be (Dimless)

        // Product
        (cm*min) shouldBe a [TypelessProductUnit]
        (C*m).asTypeless shouldBe a [TypelessProductUnit]
        // Quotient
        (km/L) shouldBe a [TypelessQuotientUnit]
        (cm/m) shouldBe a [TypelessQuotientUnit]
        (km/h).asTypeless shouldBe a [TypelessQuotientUnit]
        // Reciprocal
        km.reciprocal shouldBe a [TypelessReciprocalUnit]
        (1/km) shouldBe a [TypelessReciprocalUnit]  // require importing waman.multiverse.typeless.implicits._
        km^(-1) shouldBe a [TypelessReciprocalUnit]
        // Power
        (mm^4) shouldBe a [TypelessPowerUnit]
        km3.asTypeless shouldBe a [TypelessPowerUnit]
        m2.asTypeless shouldBe a [TypelessPowerUnit]
        m3.asTypeless shouldBe a [TypelessPowerUnit]
        s2.asTypeless shouldBe a [TypelessPowerUnit]
      }
    }

    "Properties" - {

      "the name property should have the proper value" in {
        val conversions =
          Table(
            ("unit", "expected"),
            (Dimless, "dimensionless unit"),  // Dimless
            (cm*min, "centimetre times minute"),  // product
            (km/L, "kilometre per litre"),  // quotient
            (km.reciprocal, "one per kilometre"),  //reciprocal
            // power
            (km^0, "dimensionless unit"),
            (km^1, "kilometre"),
            (km^2, "square kilometre"),
            (km^3, "cubic kilometre"),
            (km^4, "4th power of kilometre"),
            (km^(-1), "one per kilometre"),
            (km^(-2), "one per square kilometre"),
            (km^(-3), "one per cubic kilometre"),
            (km^(-4), "one per 4th power of kilometre"),

            // product
            ((cm*min)*L, "centimetre times minute times litre"),
            ((cm*min)/L, "centimetre times minute per litre"),
            ((cm*min)/cm, "minute"),
            ((cm*min)/min, "centimetre"),

            // quotient
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
            (cm*min, "cm*min"),  // product
            (km/L, "km/L"),  // quotient
            (km.reciprocal, "km⁻¹"),  //reciprocal
            (km^4, "km⁴"),  // power

            // more complex
            // product
            ((cm*min)*L, "cm*min*L"),
            ((cm*min)/L, "cm*min/L"),
            ((cm*min)/cm, "min"),
            ((cm*min)/min, "cm"),
            (cm*cm3, "cm⁴"),
            (cm3*cm, "cm⁴"),
            (cm2*cm3, "cm⁵"),

            // quotient
            ((km/L)*L, "km"),
            ((km/L)*cm3, "km*cm³/L"),
            ((km/L)/m, "km/(L*m)"),
            ((km/L)/km, "L⁻¹"),
            (cm3/cm, "cm²"),
            (cm2/cm3, "cm⁻¹"),
            (cm/cm3, "cm⁻²"),
            (cm2/(cm^5), "cm⁻³"),

            // power
            ((cm*min)*min, "cm*min²"),

            // more more complex
            ((cm*min)*min/min, "cm*min"),
            ((cm*min)*min/cm, "min²"),
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

      "the dimension property should have the proper value" in {
        import org.waman.multiverse.{DimensionSymbol => DS}
        val conversions =
          Table(
            ("unit", "expected"),
            (Dimless, Map()),  // Dimless
            (cm*min, Map(DS.L -> 1, DS.T -> 1)),  // product
            (km/L, Map(DS.L -> -2)),  // quotient
            (km.reciprocal, Map(DS.L -> -1)),  //reciprocal
            (km^4, Map(DS.L -> 4))  // power
          )

        forAll(conversions){ (unit: TypelessLinearUnit, expected: Map[_ <: DS, Int]) =>
          // Exercise
          val sut = unit.dimension
          //Verify
          sut should equal (expected)
        }
      }

      "the getSIUnit method should have the proper unit instance" in {
        import org.waman.multiverse.typeless.implicits._
        val conversions =
          Table(
            ("unit", "expected"),
//            (Dimless, Dimless),  // Dimless
//            (cm*min, m*s), // product
            (km/L, m^(-2)),  // quotient
            (cm/m, Dimless),
            (1/km, m^(-1)),  //reciprocal
            (km^4, m^4),  // power

            // more complex
            // product
            ((cm*min)*L, (m^4)*s),
            ((cm*min)/L, m*s/m3),
            ((cm*min)/cm, s),
            ((cm*min)/min, m),

            // quotient
            (km/L, m/m3),
            ((km/L)*L, m),
            ((km/L)*cm3, m),
            ((km/L)/m, 1/m3),
            ((km/L)/km, 1/m3)
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
        val sut = v.asTypeless
        // Verify
        sut shouldBe a [TypelessProductUnit]
        (sut ~= v) should be (true)
      }
    }

    "TypelessQuotientUnit should be equivalent to the ordinal quotient unit" in {
      // SetUp
      val v = m/s
      // Exercise
      val sut = v.asTypeless
      // Verify
      sut shouldBe a [TypelessQuotientUnit]
      (sut ~= v) should be (true)
    }
  }

  "Quantity" - {

    "apply method" - {

      "3.0(km/L) should be converted to the equivalent value in another unit" in {
        // SetUp
        val q = 3.0 (km/L)
        // Exercise
        val sut = q(m/m3)
        // Verify
        sut should equal (%%%%(3e6))
      }

      "apply method should throw an IllegalArgumentException when the argument has the different dimension" in {
        // SetUp
        val q = 3.0 (km/L)
        // Exercise
        a [IllegalArgumentException] should be thrownBy {
          q(km/m2)
        }
      }
    }

    "+ operator" - {

      "3.0(km/L) should be converted to the equivalent value in another unit" in {
        // SetUp
        val q0 = 3.0 (km/L)
        val q1 = 5.0 (m/L)
        // Exercise
        val sut = q0 + q1
        // Verify
        sut(km/L) should equal (%%%%(3.005))
      }

      "+ operator should throw an IllegalArgumentException when the argument has the different dimension" in {
        // SetUp
        val q0 = 3.0 (km/L)
        val q1 = 5.0 (km/m2)
        // Exercise
        a [IllegalArgumentException] should be thrownBy {
          q0 + q1
        }
      }
    }
  }
}
