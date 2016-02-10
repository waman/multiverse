package org.waman.multiverse

import org.waman.scalatest_util.ImplicitConversion
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

class UnitSystemSpec extends MultiverseCustomSpec{

  """UnitSystem#getSupportedUnits() should work well
    | with arg any of Strings returned by UnitSystem#getSupportedQuantities()""".stripMargin in {

    noException should be thrownBy {
      UnitSystem.getSupportedQuantities.foreach{ q =>
        UnitSystem.getSupportedUnits(q)
      }
    }
  }

  "UnitSystem#getSupportedContext method should return all values of Context" in {
    import Context._
    __Exercise__
    val sut = UnitSystem.getSupportedContext
    __Verify__
    sut should contain theSameElementsAs Seq(
      UnitedStates,
      Imperial
      )
  }

  "ImplicitConversions" - {

    "AngularVelocity should be implicitly converted to Frequency" taggedAs ImplicitConversion ignore {
      noException should be thrownBy{
        convertImplicitly[Frequency[Double]](1.0 rad/s)
      }
    }

    "Frequency should be implicitly converted to AngularVelocity" taggedAs ImplicitConversion ignore {
      noException should be thrownBy{
        convertImplicitly[AngularVelocity[Double]](1.0 Hz)
      }
    }
  }
}
