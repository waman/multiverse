package org.waman.multiverse

import scala.language.postfixOps

class MKSUnitSystemSpec extends MultiverseCustomSpec with MKSUnitSystem{

  """MKSUnitSystem class should has the proper number of conversion methods
    | (methods whose name starts with "convert" and ends with "ToFractional")""".stripMargin in {
    __Exercise__
    val actual = MKSUnitSystem.getClass.getDeclaredMethods
        .map(_.getName)
        .filter(s => s.startsWith("convert") && s.endsWith("ToFractional"))
        .map{ s =>
          val len = s.length
          s.substring("convert".length, len - "ToFractional".length)
        }
    __Verify__
    actual should contain theSameElementsAs UnitSystem.getSupportedQuantities
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
