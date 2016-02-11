package org.waman.multiverse

import org.waman.multiverse.Context._

import scala.language.postfixOps

class ContextSpec extends MultiverseCustomSpec{

  "UnitSystem#getSupportedContext method should return all values of Context" in {
    __Exercise__
    val sut = Context.values
    __Verify__
    sut should contain theSameElementsAs Seq(
      UnitedStates, UnitedStates_Fluid, UnitedStates_Dry, UnitedStates_Dry_Level,
      Imperial, Admiralty,

      Cu_KAlpha1, Mo_KAlpha1
    )
  }
}
