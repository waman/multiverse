package org.waman.multiverse

import org.waman.scalatest_util.ImplicitConversion
import scala.language.postfixOps

class MKSUnitSystemSpec extends MultiverseCustomSpec with MKSUnitSystem{

  "Length" - {

    "m property called on a Double value should return a Length in metre" taggedAs ImplicitConversion ignore {
      __Verify__
      noException should be thrownBy {
        convertImplicitly[Length[Double]](1.0 m)
      }
    }
  }
}
