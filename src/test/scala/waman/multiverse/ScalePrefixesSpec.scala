package waman.multiverse

import spire.implicits._
import spire.math._

class ScalePrefixesSpec extends MultiverseCustomSpec{

  "Scale prefix methods without parameter" - {

    "Double" in {
      // Exercise
      val sut = ScalePrefixes.mega[Double]
      // Verify
      sut should be (%%%%(1e6))
    }

    "Rational" in {
      // Exercise
      val sut = ScalePrefixes.peta[Rational]
      // Verify
      sut should be (r"1e15")
    }
  }

  "Scale prefix methods with parameter" - {

    "Double" in {
      // Exercise
      val sut = ScalePrefixes.mega(3.0)
      // Verify
      sut should be (%%%%(3.0e6))
    }

    "Rational" in {
      // Exercise
      val sut = ScalePrefixes.peta(r"3.0")
      // Verify
      sut should be (r"3.0e15")
    }
  }
}
