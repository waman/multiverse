package org.waman.multiverse

import org.waman.scalatest_util.{WamanCustomSpec, ImplicitConversion}
import org.waman.worldsheet.physics.WorldSheetPhysicsCustomSpec
import org.waman.worldsheet.physics.unit.MKSUnitSystem._
import scala.language.postfixOps

class MKSUnitSystemSpec extends MultiverseCustomSpec{

  def %(value: Double) = value +- (value / 100.0)

  "Length" - {

    "m method called on a Double value should return a Length in metre" taggedAs ImplicitConversion ignore {
      __Verify__
      noException should be thrownBy{
        convertImplicitly[Length](1.0 m)
      }
    }

    "m method should return a value of length in metre" in {
      __SetUp__
      val x = 1000.0 cm;
      __Exercise__
      x should be (a [Length])
      (x cm) should equal (%(1000.0))
      (x m) should equal (%(10.0))
      (x km) should equal (%(0.01))
    }

    "length unit can be specified like property access" in {
      __SetUp__
      val x = 1000.0.cm
      __Exercise__
      x should be (a [Length])
      (x cm) should equal (%(1000.0))
      (x m) should equal (%(10.0))
      (x km) should equal (%(0.01))
    }

    "length unit can be enclosed by parenthesis" in {
      import org.waman.worldsheet.physics.unit.UnitSystem._
      __SetUp__
      val x = 1000.0 (cm)
      __Exercise__
      x should be (a [Length])
      (x cm) should equal (%(1000.0))
      (x m) should equal (%(10.0))
      (x km) should equal (%(0.01))
    }

    "Length object should be implicitly converted to a Double value in metre" in {
      __SetUp__
      val x: Double = 1.0 cm;
      __Exercise__
      x should equal (%(0.01))
    }
  }

  "Time" - {

    "s method called on a Double value should return a Time in second" taggedAs ImplicitConversion ignore {
      __Verify__
      noException should be thrownBy{
        convertImplicitly[Time](1.0 s)
      }
    }

    "s method should return a value of Time in second" in {
      __SetUp__
      val t = 1.0 s;
      __Exercise__
      t should be (a [Time])
      (t ms) should equal (%(1000.0))
      (t s) should equal (%(60.0))
      (t min) should equal (%(1.0))
    }

    "time unit can be specified like property access" in {
      __SetUp__
      val t = 1.0.s
      __Exercise__
      t should be (a [Length])
      (t ms) should equal (%(1000.0))
      (t s) should equal (%(1.0))
      (t min) should equal (%(1/60.0))
    }

    "time unit can be enclosed by parenthesis" in {
      import org.waman.worldsheet.physics.unit.UnitSystem._
      __SetUp__
      val t = 1.0 (s)
      __Exercise__
      t should be (a [Length])
      (t ms) should equal (%(1000.0))
      (t s) should equal (%(1.0))
      (t min) should equal (%(1/60.0))
    }

    "Time object should be implicitly converted to a Double value in metre" in {
      __SetUp__
      val x: Double = 1.0 cm;
      __Exercise__
      x should equal (%(0.01))
    }
  }

  "Velocity" - {

    "`m/s` method called on a Double value should return a Velocity in m/s" taggedAs ImplicitConversion ignore {
      __Verify__
      noException should be thrownBy{
        convertImplicitly[Velocity](1.0 `m/s`)
      }
    }

    "`km/h` method should return a value of Velocity in km/h" in {
      __SetUp__
      val v = 72.0 `km/h`;
      __Exercise__
      v should be (a [Velocity])
      (v `m/s`) should equal (%(20.0))
      (v `km/h`) should equal (%(72.0))
    }

    "velocity unit can be specified like property access" in {
      __SetUp__
      val v = 72.0.`km/h`
      __Exercise__
      v should be (a [Velocity])
      (v `m/s`) should equal (%(20.0))
      (v `km/h`) should equal (%(72.0))
    }

    "velocity unit can be enclosed by parenthesis" in {
      import org.waman.worldsheet.physics.unit.UnitSystem._
      __SetUp__
      val v = 72.0 (`km/h`)
      __Exercise__
      v should be (a [Velocity])
      (v `m/s`) should equal (%(20.0))
      (v `km/h`) should equal (%(72.0))
    }

    "Velocity object should be implicitly converted to a Double value in metre" in {
      __SetUp__
      val v: Double = 72.0 `km/h`;
      __Exercise__
      v should equal (%(20.0))
    }

    "Velocity object should be created by dividing Length by Time" in {
      import UnitSystem._
      __SetUp__
      val v = 20.0 m/s;
      __Exercise__
      (v `m/s`) should equal (%(20.0))
      (v `km/h`) should equal (%(72.0))
    }

    "Velocity object should be created by dividing Length by Time (property access)" in {
      import UnitSystem._
      __SetUp__
      val v = 20.0.m/s
      __Exercise__
      (v `m/s`) should equal (%(20.0))
      (v `km/h`) should equal (%(72.0))
    }
  }
}
