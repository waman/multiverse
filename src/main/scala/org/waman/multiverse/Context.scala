package org.waman.multiverse

sealed abstract class Context(val symbol: String){
  lazy val name = {
    val s = getClass.getSimpleName
    s.substring(0, s.length - 1)
  }
}

object Context extends ConstantsDefined[Context]{
  /** The "US" context contains the "US Survey" one for Length and Area (i.e. ft(US) and mi(US)) */
  case object UnitedStates           extends Context("US")
  case object UnitedStates_Fluid     extends Context("US_fl")
  case object UnitedStates_Dry       extends Context("US_dry")
  case object UnitedStates_Dry_Level extends Context("US_lvl")

  case object Imperial  extends Context("imp")
  case object Admiralty extends Context("Adm")

  case object Cu_KAlpha1 extends Context("CuKα1")
  case object Mo_KAlpha1 extends Context("MoKα1")

  lazy val values = Seq(
    UnitedStates,
    UnitedStates_Fluid,
    UnitedStates_Dry,
    UnitedStates_Dry_Level,
    Imperial,
    Admiralty,

    Cu_KAlpha1,
    Mo_KAlpha1
  )
}

trait HasContext{
  import Context._

  val US     = UnitedStates
  val US_fl  = UnitedStates_Fluid
  val US_dry = UnitedStates_Dry
  val US_lvl = UnitedStates_Dry_Level

  val imp = Imperial
  val Adm = Admiralty

  val CuKα1 = Cu_KAlpha1
  val MoKα1 = Mo_KAlpha1
}