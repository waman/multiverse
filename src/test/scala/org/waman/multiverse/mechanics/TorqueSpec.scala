package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TorqueSpec extends MultiverseCustomSpec with PropertyChecks{

  "TorqueUnit should" - {

    "return a torque value of 1  in Newton metre by 'inNewtonMetre' property" in {
      __SetUp__
      val tu = N*m
      __Verify__
      tu.unitInNewtonMetre should equal (r"1.0")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (N*m) should equal (N*m)
      (N*m).hashCode should equal ((N*m).hashCode)
    }
  }

  "Predefined torque units" - {

    "3.0 <<torque unit>> should be converted to the equivalent value in Newton metre" in {
      __Exercise__
      val conversions =
        Table(
          ("torques", "expected"),
          (Seq(3.0.N * m, 3.0 N * m, 3.0 (N * m)), 3.0)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Torque[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut N * m) should equal(%%%%(expected))
        }
      }
    }

    "3.0 N*m should be converted to the equivalent value in other torque units" in {
      __SetUp__
      val q = 3.0 (N * m)
      __Exercise__
      val conversions =
        Table(
          ("torques", "expected"),
          (Seq(q.N * m, q N * m, q(N * m)), 3.0)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }
}
