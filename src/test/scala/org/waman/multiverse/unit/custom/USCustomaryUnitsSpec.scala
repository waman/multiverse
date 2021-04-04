package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs.Length
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.custom.BasicUnits.{min => _, _}
import org.waman.multiverse.unit.defs._

class USCustomaryUnitsSpec extends MultiverseCustomSpec {

  "US Customary Units" - {
    import org.waman.multiverse.unit.custom.USCustomaryUnits._

    "Length Units should have the correct values" in {
      // Exercise
      val conversions =
        Table(
          ("length", "expected"),
          (1.0(pt), (127.0/360.0) (mm)),

          (1.0(pc), (127.0/30.0) (mm)),
          (1.0(pc), 12.0 (pt)),

          (1.0(in), 25.4 (mm)),
          (1.0(in), 6.0 (pc)),

          (1.0(ft), 0.3048 (m)),
          (1.0(ft), 12.0 (in)),

          (1.0(yd), 0.9144 (m)),
          (1.0(yd), 3.0 (ft)),

          (1.0(mi), 1.609344 (km)),
          (1.0(mi), 5280.0 (ft)),
          (1.0(mi), 1760.0 (yd)),

          (1.0(ftm), (1143.0/625.0) (m)),
          (1.0(ftm), 2.0 (yd)),

          (1.0(cb), (3429.0/15625.0) (km)),
          (1.0(cb), 120.0 (ftm)),

          (1.0(NM), 1852.0 (m)),
          (1.0(nmi), 1852.0 (m))
        )
      // Verify
      forAll(conversions){ (sut: Length[Double], expected: Length[Double]) =>
        sut.getSIValue should equal (%%%%(expected.getSIValue))
      }
    }

    "Area Units should have the correct values" in {
      // Exercise
      val conversions =
        Table(
          ("area", "expected"),
          (1.0(sq_ft), 9.290304e-2 (m2)),
          (1.0(ft2), 9.290304e-2 (m2)),
          (1.0(sq_ft), 144.0 (AreaUnits.in2)),

          (1.0(sq_ch), 404.68564224 (m2)),
          (1.0(ch2), 404.68564224 (m2)),
          (1.0(sq_ch), 4356.0 (sq_ft)),
          (1.0(sq_ch), 16.0 (AreaUnits.sq_rd)),

          (1.0(ac), 4046.8564224 (m2)),
          (1.0(ac), 43560.0 (sq_ft)),
          (1.0(ac), 10.0 (AreaUnits.sq_ch)),

          (1.0(section), 2.589988110336 (km2)),
          (1.0(section), 640.0 (ac)),
          (1.0(section), 1.0 (AreaUnits.sq_mi))
        )
          // Verify
          forAll(conversions) { (sut: Area[Double], expected: Area[Double]) =>
          sut.getSIValue should equal(%%%%(expected.getSIValue))
        }
    }

    "Volume Units should have the correct values" in {
      // Exercise
      val conversions =
        Table(
          ("volume", "expected"),
          (1.0(in3), 16.387064 (mL)),
          (1.0(cu_in), 16.387064 (mL)),

          (1.0(ft3), 28.316846592 (L)),
          (1.0(cu_ft), 28.316846592 (L)),
          (1.0(cu_ft), 1728.0 (cu_in)),

          (1.0(yd3), 764.554857984 (L)),
          (1.0(cu_yd), 764.554857984 (L)),
          (1.0(cu_yd), 27.0 (cu_ft)),

          (1.0(acre_ft), 1.23348183754752e6 (L)),
          (1.0(acre_ft), 43560.0 (cu_ft))
        )
      // Verify
      forAll(conversions) { (sut: Volume[Double], expected: Volume[Double]) =>
        sut.getSIValue should equal(%%%%(expected.getSIValue))
      }
    }

    "Mass Units should have the correct values" in {
      import org.waman.multiverse.unit.defs.MassUnits._
      // Exercise
      val conversions =
        Table(
          // Avoirdupois
          ("length", "expected"),
          (1.0(gr), 64.79891 (mg)),
          (1.0(gr), (1.0/7000.0) (MassUnits.lb)),
          (1.0(gr), (1.0/5760.0) (MassUnits.lb(troy))),

          (1.0(dr), 1.7718451953125 (g)),
          (1.0(dr), (27.0 + 11.0/32.0) (gr)),

          (1.0(oz), 28.349523125 (g)),
          (1.0(oz), 16.0 (dr)),

          (1.0(lb), 453.59237 (g)),
          (1.0(lb), 16.0 (oz)),

          (1.0(cwt), 45.359237 (kg)),
          (1.0(cwt), 100.0 (lb)),

          (1.0(long_cwt), 50.80234544 (kg)),
          (1.0(long_cwt), 112.0 (lb)),

          (1.0(sh_tn), 907.18474 (kg)),
          (1.0(sh_tn), 20.0 (cwt)),
          (1.0(sh_tn), 2000.0 (lb)),

          (1.0(long_tn), 1016.0469088 (kg)),
          (1.0(long_tn), 20.0 (long_cwt)),
          (1.0(long_tn), 2240.0(lb)),

          // Troy
          (1.0(dwt), 1.55517384 (g)),
          (1.0(dwt), 24.0(gr)),

          (1.0(oz_t), 31.1034768 (g)),
          (1.0(oz_t), 20.0(dwt)),

          (1.0(lb_t), 373.2417216 (g)),
          (1.0(lb_t), 12.0(oz_t))
        )

      // Verify
      forAll(conversions) { (sut: Mass[Double], expected: Mass[Double]) =>
        sut.getSIValue should equal(%%%%(expected.getSIValue))
      }
    }
  }

  "Survey Units" - {
    import org.waman.multiverse.unit.custom.USCustomarySurveyUnits._

    "Length Units should have the correct values" in {
      // Exercise
      val conversions =
        Table(
          ("length", "expected"),
          (1.0(li), (792.0 / 3937.0) (m)),
          (1.0(li), (33.0/50.0) (ft)),

          (1.0(ft), (1200.0 / 3937.0) (m)),

          (1.0(rd), (19800.0 / 3937.0) (m)),
          (1.0(rd), 25.0 (li)),
          (1.0(rd), 16.5 (ft)),

          (1.0(ch), (79200.0 / 3937.0) (m)),
          (1.0(ch), 4.0 (rd)),
          (1.0(ch), 66.0 (ft)),

          (1.0(fur), 10.0 (ch)),

          (1.0(mi), (6336.0 / 3937.0) (km)),
          (1.0(mi), 8.0 (fur)),

          (1.0(lea), (19008.0 / 3937.0) (km)),
          (1.0(lea), 3.0 (mi))
        )

      // Verify
      forAll(conversions) { (sut: Length[Double], expected: Length[Double]) =>
        sut.getSIValue should equal(%%%%(expected.getSIValue))
      }
    }

    "Area Units should have the correct values" in {
      import org.waman.multiverse.unit.defs.MetricAttributes._
      // Exercise
      val conversions =
        Table(
          ("area", "expected"),
          (1.0(sq_ft), 1.0 (AreaUnits.sq_ft(US))),
          (1.0(ft2), 1.0 (AreaUnits.sq_ft(US))),

          (1.0(sq_ch), 1.0 (AreaUnits.sq_ch(US))),
          (1.0(ch2), 1.0 (AreaUnits.sq_ch(US))),
          (1.0(sq_ch), 4356.0 (sq_ft)),
          (1.0(sq_ch), 16.0 (AreaUnits.sq_rd(US))),

          (1.0(ac), 1.0 (AreaUnits.ac(US))),
          (1.0(ac), 43560.0 (sq_ft)),
          (1.0(ac), 10.0 (sq_ch)),

          (1.0(section), 1.0 (AreaUnits.section(US))),
          (1.0(section), 640.0 (ac)),
          (1.0(section), 1.0 (AreaUnits.sq_mi(US))),

          (1.0(twp), 36.0 (section)),
          (1.0(twp), 4.0 (lea.squared))
        )

      // Verify
      forAll(conversions) { (sut: Area[Double], expected: Area[Double]) =>
        sut.getSIValue should equal(%%%%(expected.getSIValue))
      }
    }
  }

  "Fluid Units" - {
    import org.waman.multiverse.unit.custom.USCustomaryFluidUnits._

    "Fluid volume Units should have the correct values" in {
      // Exercise
      val conversions =
        Table(
          ("volume", "expected"),
          (1.0(min), 61.611519921875e-6 (L)),

          (1.0(fl_dr), 3.6966911953125 (mL)),
          (1.0(fl_dr), 60.0 (min)),

          (1.0(tsp), 4.92892159375 (mL)),
          (1.0(tsp), 80.0 (min)),

          (1.0(Tbsp), 14.78676478125 (mL)),
          (1.0(Tbsp), 3.0 (tsp)),
          (1.0(Tbsp), 4.0 (fl_dr)),

          (1.0(fl_oz), 29.5735295625 (mL)),
          (1.0(fl_oz), 2.0 (Tbsp)),

          (1.0(jig), 44.36029434375 (mL)),
          (1.0(jig), 1.5 (fl_oz)),
          (1.0(jig), 3.0 (Tbsp)),

          (1.0(gi), 118.29411825 (mL)),
          (1.0(gi), (8.0/3.0) (jig)),
          (1.0(gi), 4.0 (fl_oz)),

          (1.0(cp), 236.5882365 (mL)),
          (1.0(cp), 2.0 (gi)),
          (1.0(cp), 8.0 (fl_oz)),

          (1.0(pt), 473.176473 (mL)),
          (1.0(pt), 2.0 (cp)),

          (1.0(qt), 0.946352946 (L)),
          (1.0(qt), 2.0 (pt)),

          (1.0(pot), 1.89270589 (L)),
          (1.0(pot), 2.0 (qt)),

          (1.0(gal), 3.785411784 (L)),
          (1.0(gal), 4.0 (qt)),
          (1.0(gal), 231.0 (VolumeUnits.cu_in)),

          (1.0(fl_bl), 119.240471196 (L)),
          (1.0(fl_bl), 31.5 (gal)),
          (1.0(fl_bl), 0.5 (hhd)),

          (1.0(bbl), 158.987294928 (L)),
          (1.0(bbl), (4.0/3.0) (fl_bl)),
          (1.0(bbl), 42.0 (gal)),
          (1.0(bbl), (2.0/3.0) (hhd)),

          (1.0(hhd), 238.480942392 (L)),
          (1.0(hhd), 1.5 (bbl)),
          (1.0(hhd), 63.0 (gal)),
          (1.0(hhd), 8.421875 (VolumeUnits.cu_ft))
        )

      // Verify
      forAll(conversions) { (sut: Volume[Double], expected: Volume[Double]) =>
        sut.getSIValue should equal(%%%%(expected.getSIValue))
      }
    }
  }

  "Dry Units" - {
    import org.waman.multiverse.unit.custom.USCustomaryDryUnits._

    "Dry volume Units should have the correct values" in {
      // Exercise
      val conversions =
        Table(
          ("volume", "expected"),
          (1.0(pt), 0.5506104713575(L)),
          (1.0(pt), 33.6003125(VolumeUnits.cu_in)),

          (1.0(qt), 1.101220942715(L)),
          (1.0(qt), 2.0(pt)),

          (1.0(gal), 4.40488377086(L)),
          (1.0(gal), 4.0(qt)),

          (1.0(pk), 8.80976754172(L)),
          (1.0(pk), 2.0(gal)),

          (1.0(bu), 35.23907016688(L)),
          (1.0(bu), 4.0(pk)),

          (1.0(bbl), 115.628198985075(L)),
          (1.0(bbl), 105.0(qt))
        )

      // Verify
      forAll(conversions) { (sut: Volume[Double], expected: Volume[Double]) =>
        sut.getSIValue should equal(%%%%(expected.getSIValue))
      }
    }
  }
}