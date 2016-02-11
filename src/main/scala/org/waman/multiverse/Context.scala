package org.waman.multiverse

sealed abstract class Context(val symbol: String)

object Context{
  /** The "US" context contains the "US Survey" one for Length and Area (i.e. ft(US) and mi(US)) */
  case object UnitedStates           extends Context("US")
  case object UnitedStates_Fluid     extends Context("US_fl")
  case object UnitedStates_Dry       extends Context("US_dry")
  case object UnitedStates_Dry_Level extends Context("US_lvl")
  case object Imperial               extends Context("imp")
  case object Admiralty              extends Context("Adm")

  case object Cu_KAlpha1 extends Context("CuKα1")
  case object Mo_KAlpha1 extends Context("MoKα1")

  lazy val values: Seq[Context] = Seq(
    UnitedStates, UnitedStates_Fluid, UnitedStates_Dry, UnitedStates_Dry_Level,
    Imperial, Admiralty,

    Cu_KAlpha1, Mo_KAlpha1
  )
}

trait HasContext{
  val US     = Context.UnitedStates
  val US_fl  = Context.UnitedStates_Fluid
  val US_dry = Context.UnitedStates_Dry
  val US_lvl = Context.UnitedStates_Dry_Level

  val imp = Context.Imperial
  val Adm = Context.Admiralty

  val CuKα1 = Context.Cu_KAlpha1
  val MoKα1 = Context.Mo_KAlpha1
}