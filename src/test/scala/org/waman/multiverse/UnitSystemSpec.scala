package org.waman.multiverse

import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.angle.AngularVelocity
import org.waman.multiverse.time.Frequency
import org.waman.scalatest_util.ImplicitConversion

import scala.language.postfixOps

class UnitSystemSpec extends MultiverseCustomSpec{

  "supportedQuantities property should return a set of Class objects of supported Quantities" in {
    __SetUp__
    val expected = Set(
      "Length"      , "Area"            , "Volume"            , "Angle"        , "SolidAngle"       ,
      "Mass"        , "Density"         , "Time"              , "TimeSquared"  , "Frequency"        ,
      "Velocity"    , "AngularVelocity" , "VolumeFlow"        , "Acceleration" , "Force"            ,
      "Pressure"    , "Torque"          , "Energy"            , "Momentum"     , "Power"            ,
      "Action"      , "DynamicViscosity", "KinematicViscosity", "Current"      , "Charge"           ,
      "Dipole"      , "Voltage"         , "Resistance"        , "Capacitance"  , "Flux"             ,
      "FluxDensity" , "Inductance"      , "Temperature"       , "Entropy"      , "LuminousIntensity",
      "Luminance"   , "LuminousFlux"    , "Illuminance"       , "Radioactivity", "Exposure"         ,
      "AbsorbedDose", "EquivalentDose"  , "EquivalentDoseRate"
    )
    __Exercise__
    val sut = UnitSystem.supportedQuantities.map(_.getSimpleName)
    __Verify__
    sut should containTheSameElementsAs (expected)
  }

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
//    __Exercise__
//    val sut = UnitSystem.getSupportedContext(classOf[LengthUnit], "mi")
//    __Verify__
//    sut should contain theSameElementsAs Seq(Context.UnitedStates)
//  }

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
