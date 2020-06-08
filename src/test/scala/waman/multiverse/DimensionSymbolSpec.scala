package waman.multiverse

import waman.multiverse.unit.basic.LengthUnit
import waman.multiverse.unit.electromagnetism.VoltageUnit
import waman.multiverse.unit.photometry.LuminanceUnit
import waman.multiverse.unit.mechanics.{EnergyUnit, TimeSquaredUnit}
import waman.multiverse.unit.thermodynamics.TemperatureUnit

class DimensionSymbolSpec extends MultiverseCustomSpec{

  "toStringWithUnit method should return the proper string" in {
    // SetUp
    val conversions =
      Table(
        ("unit", "expected"),
        (LengthUnit.dimension, "m"),
        (TimeSquaredUnit.dimension, "s²"),
        (EnergyUnit.dimension, "kg m² s⁻²"),
        (VoltageUnit.dimension, "kg m² s⁻³ A⁻¹"),
        (LuminanceUnit.dimension, "m⁻² cd"),
        (TemperatureUnit.dimension, "K")
      )
    // Verify
    forAll(conversions){ (dim: Map[DimensionSymbol, Int], expected: String) =>
      // Exercise
      val sut = DimensionSymbol.toStringWithUnit(dim)
      // Verify
      sut should equal (expected)
    }
  }

  "toStringWithSymbol method should return the proper string" in {
    // SetUp
    val conversions =
      Table(
        ("unit", "expected"),
        (LengthUnit.dimension, "L"),
        (TimeSquaredUnit.dimension, "T²"),
        (EnergyUnit.dimension, "ML²T⁻²"),
        (VoltageUnit.dimension, "ML²T⁻³I⁻¹"),
        (LuminanceUnit.dimension, "L⁻²J"),
        (TemperatureUnit.dimension, "Θ")
      )
    // Verify
    forAll(conversions){ (dim: Map[DimensionSymbol, Int], expected: String) =>
      // Exercise
      val sut = DimensionSymbol.toStringWithSymbol(dim)
      // Verify
      sut should equal (expected)
    }
  }
}
