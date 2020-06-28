package waman.multiverse.unit

import spire.math.Real
import spire.implicits._

object Constants{
  val Pi: Real = Real.pi
  val BohrRadius: Real = r"5.2917721090380e-11"
  val SpeedOfLight: Real = r"299792458"
  val GravitationalConstant: Real = r"6.6743015e-11"
  val ElectronMass: Real = r"9.1093829140e-31"
  val AtomicMassUnit: Real = r"1.6605390666050e-27"
  val RydbergConstant: Real = r"10973731.56816021"
  val PlanckConstant: Real = r"6.62607015e-34"
  val ReducedPlanckConstant: Real = PlanckConstant / (Real.two * Real.pi)
  val ElementaryCharge: Real = r"1.602176634e-19"
  val CoulombConstant: Real = r"8.987551792314e9"
  val BohrMagneton: Real = r"9.27400999457e-24"
  val VacuumPermittivity: Real = r"8.854187812813e-12"
  val FineStructureConstant: Real = r"0.007297352569311"
  val BoltzmannConstant: Real = r"1.380649e-23"
  val AvogadroConstant: Real = r"6.02214076e23"
}