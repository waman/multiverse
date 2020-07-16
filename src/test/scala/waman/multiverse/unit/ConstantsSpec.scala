package waman.multiverse.unit

import spire.implicits._
import spire.math.Real
import waman.multiverse.MultiverseCustomSpec
import waman.multiverse.unit.Constants._
import waman.multiverse.unit.basic.TimeUnitObjects
import waman.multiverse.unit.electromagnetism.{ElectricChargeUnitObjects, ElectricDipoleUnitObjects}
import waman.multiverse.unit.mechanics.{AngularMomentumUnits, EnergyUnitObjects, ForceUnitObjects}

class ConstantsSpec extends MultiverseCustomSpec {

  "a_0 (Bohr radius) ~ ħ/m_e cα" in {
    val sut = Constants.BohrRadius.toDouble
    val expected = (PlanckConstant / (ElectronMass * SpeedOfLight * FineStructureConstant * Real.two * Real.pi)).toDouble
    sut should equal (%%%(expected))
    sut should equal (%%%%(5.2917721090380e-11))
  }

  "Planck time ~ √(Gh/2πc^5)" in {
    val sut = TimeUnitObjects.planck_time.interval.toDouble
    val expected = (GravitationalConstant * PlanckConstant / (Real.two * Real.pi * (SpeedOfLight**5))).sqrt.toDouble
    sut should equal (%%%%(expected))
    sut should equal (%%(5.39116e-44))
  }

  "R∞ (rydberg constant) ~ m_e e^4 / (8ε_0^2 h^3 c)" in {
    val sut = Constants.RydbergConstant.toDouble
    val expected = (ElectronMass * (ElementaryCharge**4) / ((VacuumPermittivity**2) * (PlanckConstant**3) * SpeedOfLight * 8 )).toDouble
    sut should equal (%%%(expected))
  }

  "k (Coulomb constant) ~ 1 / (4πε_0)" in {
    val sut = Constants.CoulombConstant.toDouble
    val expected = (Real.one / (Real.pi * VacuumPermittivity * 4 )).toDouble
    sut should equal (%%%(expected))
  }

  "α ~ 1/137" in {
    FineStructureConstant.toDouble should equal (%(1.0/137.0))
  }

  "e * N_A ~ 96500" in {
    val sut = (ElementaryCharge * AvogadroConstant).toDouble
    sut should equal (%%%(96485.3383))
  }

  "Bohr magneton ~ eħ/2m_e" in {
    val sut = Constants.BohrMagneton.toDouble
    val expected = (ElementaryCharge * PlanckConstant / (ElectronMass * Real(4) * Real.pi)).toDouble
    sut should equal (%%%(expected))
    sut should equal (%%%%(9.27400999457e-24))
  }

  "atomic units" - {

    "atomic unit of time ~ a_0/(α*c)" in {
      val sut = TimeUnitObjects.atomic_unit_of_time.interval.toDouble
      val expected = (BohrRadius / (FineStructureConstant * SpeedOfLight)).toDouble
      sut should equal (%%%(expected))
      sut should equal (%%%(2.418884254e-17))
    }

    "atomic unit of energy ~ m_e α^2 c^2" in {
      val sut = EnergyUnitObjects.atomic_unit_of_energy.interval.toDouble
      val expected = (ElectronMass * (FineStructureConstant**2) * (SpeedOfLight**2)).toDouble
      sut should equal (%%%(expected))
      sut should equal (%%%(4.359744e-18))
    }

    "atomic unit of force ~ E_h / a_0" in {
      val sut = ForceUnitObjects.atomic_unit_of_force.interval.toDouble
      val expected = (EnergyUnitObjects.atomic_unit_of_energy.interval / BohrRadius).toDouble
      sut should equal (%%%(expected))
      sut should equal (%%%(8.23872206e-8))
    }

    "atomic unit of action ~ ħ" in {
      val sut = AngularMomentumUnits.atomic_unit_of_action.interval.toDouble
      val expected = (PlanckConstant / (Real.two * Real.pi)).toDouble
      sut should equal (%%%(expected))
      sut should equal (%%%(1.05457168e-34))
    }

    "atomic unit of charge ~ e" in {
      val sut = ElectricChargeUnitObjects.atomic_unit_of_charge.interval.toDouble
      val expected = ElementaryCharge.toDouble
      sut should equal (%%%%(expected))
      sut should equal (%%%%(1.602176634e-19))
    }

    "atomic unit of dipole moment ~ e" in {
      val sut = ElectricDipoleUnitObjects.atomic_unit_of_electric_dipole_moment.interval.toDouble
      val expected = (ElementaryCharge * BohrRadius).toDouble
      sut should equal (%%%%(expected))
      sut should equal (%%%(8.47835281e-30))
    }
  }

  "Physical constants in CGS" in {
    import waman.multiverse.implicits._
    import waman.multiverse.unit.BasicUnits._
    import waman.multiverse.unit.basic.MassUnits.u
    import waman.multiverse.unit.basic.LengthUnits.a0
    import waman.multiverse.unit.MechanicalUnits._
    import waman.multiverse.unit.ElectromagneticUnits._
    // Exercise
    val conversions =
      Table(
        ("current", "expected"),
        (1(u)(g), 1.6605390666050e-24),
        (BohrMagneton(J/T)(erg/G), 9.274010078e-21),
        (BohrMagneton(J/T)(statA*cm2), 2.78027800e-10),
        (1(a0)(cm), 5.2917720859e-9),
        (BoltzmannConstant(J/K)(erg/K), 1.380649e-16),
        (ElectronMass(kg)(g), 9.10938375015e-28),
        (1(e)(statC), 4.80320427e-10),
        (1(e)(abC), 1.602176634e-20),
        (GravitationalConstant(N*m2/(kg^2))(dyn*cm2/(g^2)), 6.67430e-8),
        (PlanckConstant(J*s)(erg*s), 6.62606885e-27),
        (ReducedPlanckConstant(J*s)(erg*s), 1.054571817e-27),
        (SpeedOfLight(m/s)(cm/s), 2.99792458e10)
      )
    // Verify
    forAll(conversions) { (sut: Real, expected: Double) =>
      sut.toDouble should equal(%%%(expected))
    }
  }
}
