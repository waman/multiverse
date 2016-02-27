package org.waman.multiverse

import org.waman.multiverse.MKSUnitSystem._

import scala.language.postfixOps

class MKSUnitSystemSpec extends MultiverseCustomSpec{

  "Implementation Constraints" - {

    """MKSUnitSystem class should has the proper number of conversion methods
      | (methods whose name starts with "convert" and ends with "ToFractional")""".stripMargin in {
      __Exercise__
      val sut = MKSUnitSystem.getClass.getDeclaredMethods
        .map(_.getName)
        .filter(s => s.startsWith("convert") && s.endsWith("ToFractional"))
        .map{ s =>
          val len = s.length
          s.substring("convert".length, len - "ToFractional".length)
        }.toSet
      __Verify__
      sut should containTheSameElementsAs (UnitSystem.supportedQuantities.map(_.getSimpleName))
    }

  }

  "m property called on a Double value should return a Length in metre" in {
    __SetUp__
    val length = 1.0 km;
    __Exercise__
    val sut: Double = length
    __Verify__
    sut should equal (1000.0)
  }
}
