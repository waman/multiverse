package org.waman.multiverse.unit.radiometry

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.BasicUnits._
import org.waman.multiverse.unit.MechanicalUnits._
import org.waman.multiverse.unit.radiometry.SpectralIrradianceUnitObjects.{jansky, solar_flux_unit}
import org.waman.multiverse.unit.radiometry.SpectralIrradianceUnits._

class SpectralIrradianceSpec extends MultiverseCustomSpec {

  "Unit" - {

    "The symbol property of spectral irradiance unit should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("spectral irradiance unit", "expected"),
          (W/m2/Hz, "W/m²/Hz"),
          (W/(m2*Hz), "W/(m²*Hz)"),
          (erg/s/cm2/Hz, "erg/s/cm²/Hz"),
          (jansky, "Jy"),
          (solar_flux_unit, "sfu")
        )
      // Verify
      forAll(conversions){ (sut: SpectralIrradianceUnit, expected: String) =>
        sut.symbol should equal (expected)
      }
    }
  }

  "Quantity" - {

    "3.0 <<spectral irradiance unit>> should be converted to the equivalent value in W/(m²*Hz)" in {
      // Exercise
      val conversions =
        Table(
          ("spectral irradiance", "expected"),
          (3.0(W/(m2*Hz)), 3.0),
          (3.0(erg/s/cm2/Hz), 3e-3),
          (3.0(Jy) , 3e-26),
          (3.0(sfu), 3e-22)
        )
      // Verify
      forAll(conversions){ (sut: SpectralIrradiance[Double], expected: Double) =>
        sut(W/(m2*Hz)) should equal (%%%%(expected))
      }
    }

    "3.0(m/s2) should be converted to the equivalent value in other acceleration units" in {
      // SetUp
      val q = 3.0 (W/(m2*Hz))
      // Exercise
      val conversions =
        Table(
          ("spectral irradiance", "expected"),
          (q(W/(m2*Hz)), 3.0),
          (q(erg/s/cm2/Hz), 3e3),
          (q(Jy) , 3e26),
          (q(sfu), 3e22)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
