package org.waman.multiverse

sealed abstract class Context(val name: String, val symbol: String)

/** The "US" context contains the "US Survey" one for Length and Area (i.e. ft(US) and mi(US)) */
object Context extends ConstantsDefined[Context]{

  case object UnitedStates extends Context("UnitedStates", "US")
  case object UnitedStates_Fluid extends Context("UnitedStates_Fluid", "US_fl")
  case object UnitedStates_Dry extends Context("UnitedStates_Dry", "US_dry")
  case object UnitedStates_Dry_Level extends Context("UnitedStates_Dry_Level", "US_lvl")
  case object Imperial extends Context("Imperial", "imp")
  case object Admiralty extends Context("Admiralty", "Adm")
  case object Cu_KAlpha1 extends Context("Cu_KAlpha1", "CuKα1")
  case object Mo_KAlpha1 extends Context("Mo_KAlpha1", "MoKα1")
  lazy val values = Seq(UnitedStates, UnitedStates_Fluid, UnitedStates_Dry, UnitedStates_Dry_Level, Imperial, Admiralty, Cu_KAlpha1, Mo_KAlpha1)
}

trait HasContext{
  import Context._

  val US = UnitedStates
  val US_fl = UnitedStates_Fluid
  val US_dry = UnitedStates_Dry
  val US_lvl = UnitedStates_Dry_Level
  val imp = Imperial
  val Adm = Admiralty
  val CuKα1 = Cu_KAlpha1
  val MoKα1 = Mo_KAlpha1
}