package org.waman.multiverse

class UnitSystemSpec extends MultiverseCustomSpec{

  """UnitSystem#getSupportedUnits() should work well
    | with arg any of Strings returned by UnitSystem#getSupportedQuantities()""".stripMargin in {

    noException should be thrownBy {
      UnitSystem.getSupportedQuantities.foreach{ q =>
        UnitSystem.getSupportedUnits(q)
      }
    }
  }
}
