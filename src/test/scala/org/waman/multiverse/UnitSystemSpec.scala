package org.waman.multiverse

class UnitSystemSpec extends MultiverseCustomSpec{

//  "supportedQuantities property should return a set of Class objects of supported Quantities" in {
//    // SetUp
//    val expected = Set(
//      "Length"      , "Area"            , "Volume"            , "Angle"        , "SolidAngle"       ,
//      "Mass"        , "Density"         , "Time"              , "TimeSquared"  , "Frequency"        ,
//      "Velocity"    , "AngularVelocity" , "VolumeFlow"        , "Acceleration" , "Force"            ,
//      "Pressure"    , "Torque"          , "Energy"            , "Momentum"     , "Power"            ,
//      "Action"      , "DynamicViscosity", "KinematicViscosity", "Current"      , "Charge"           ,
//      "Dipole"      , "Voltage"         , "Resistance"        , "Capacitance"  , "Flux"             ,
//      "FluxDensity" , "Inductance"      , "Temperature"       , "Entropy"      , "LuminousIntensity",
//      "Luminance"   , "LuminousFlux"    , "Illuminance"       , "Radioactivity", "Exposure"         ,
//      "AbsorbedDose", "EquivalentDose"  , "EquivalentDoseRate"
//    )
//    // Exercise
//    val sut = UnitSystem.supportedQuantities.map(_.getSimpleName)
//    // Verify
//    sut should containTheSameElementsAs (expected)
//  }

//  """getSupportedUnits() should work well
//    | with arg any of Strings returned by UnitSystem#getSupportedQuantities()""".stripMargin in {
//
//    noException should be thrownBy {
//      UnitSystem.supportedQuantities.foreach{ q =>
//        UnitSystem.getSupportedUnits(q)
//      }
//    }
//  }
//
//  "UnitSystem#getSupportedContext() should" in {
//    // Exercise
//    val sut = UnitSystem.getSupportedContext(classOf[LengthUnit], "mi")
//    // Verify
//    sut should contain theSameElementsAs Seq(Context.UnitedStates)
//  }
//
//  "ImplicitConversions" - {
//
//    "AngularVelocity should be implicitly converted to Frequency" taggedAs ImplicitConversion ignore {
//      noException should be thrownBy{
//        convertImplicitly[Frequency[Double]](1.0 rad/s)
//      }
//    }
//
//    "Frequency should be implicitly converted to AngularVelocity" taggedAs ImplicitConversion ignore {
//      noException should be thrownBy{
//        convertImplicitly[AngularVelocity[Double]](1.0 Hz)
//      }
//    }
//  }
}
