package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class DensitySpec extends MultiverseCustomSpec with PropertyChecks{

//  "UnitSystem#getSupportedUnits method should return supported units of density" in {
//    __SetUp__
//    import DensityUnit._
//    __Exercise__
//    val result = UnitSystem.getSupportedUnits(classOf[DensityUnit])
//    __Verify__
//    result should contain ()
//  }

  "DensityUnit should" - {

    "return a density value of 1 g/cm3 in kilogram per cubic metre by 'inKilogramPerCubicMetre' property" in {
      __SetUp__
      val du = g/cm3
      __Verify__
      du.unitInKilogramPerCubicMetre should equal (r"1e3")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (kg/m3) should equal (kg/m3)
      (kg/m3).hashCode should equal ((kg/m3).hashCode)
    }
  }

  "Tests where converting from some units to m/s like 3.0 g/cm3 => 3000.0 kg/m3" in {
    val conversions =
      Table(
        ("density", "expected"),
        (Seq(3.0.kg/m3, 3.0 kg/m3, 3.0 (kg/m3)), 3.0),
        (Seq(3.0.g/cm3, 3.0 g/cm3, 3.0 (g/cm3)), 3e3),
        (Seq(3.0.g/mL , 3.0 g/mL , 3.0 (g/mL)) , 3e3),
        (Seq(3.0.kg/L , 3.0 kg/L , 3.0 (kg/L)) , 3e3)
      )

    forAll(conversions){ (ds: Seq[Density[Double]], expected: Double) =>
      ds.foreach{ d =>
        (d kg/m3) should equal (%(expected))
      }
    }
  }

  val three_kgm3 = 3.0 kg/m3

  "Tests where converting a density unit to other units like 3.0 kg/m3 => 3e-3 g/cm3" in {
    val conversions =
      Table(
        ("density", "expected"),
        (Seq(three_kgm3.kg/m3, three_kgm3 kg/m3, three_kgm3 (kg/m3)), 3.0),
        (Seq(three_kgm3.g/cm3, three_kgm3 g/cm3, three_kgm3 (g/cm3)), 3e-3),
        (Seq(three_kgm3.g/mL , three_kgm3 g/mL , three_kgm3 (g/mL)) , 3e-3),
        (Seq(three_kgm3.kg/L , three_kgm3 kg/L , three_kgm3 (kg/L)) , 3e-3)
      )

    forAll(conversions){ (vs: Seq[Double], expected: Double) =>
      vs.foreach{ v =>
        v should equal (%(expected))
      }
    }
  }
}
