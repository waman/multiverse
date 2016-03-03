package org.waman.multiverse.thermal

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TemperatureSpec
  extends AbstractQuantityAndUnitSpec[TemperatureUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[TemperatureUnit]

  "Implementation Constraint of Temperature" - {

    """DegreeTemperaturePostfixOps trait should have properties
      | whose names are the same as 'symbol' properties of TemperatureUnit objects
      |  and its value starts with '°' char""".stripMargin in {

      def getPostfixOpsPropertyNames: Seq[String] =
        classOf[DegreeTemperaturePostfixOps[_]].getMethods
            .map(_.getName)
            .filterNot(_.endsWith("PostfixOps"))
      __SetUp__
      val sut = getPostfixOpsPropertyNames
      val expected = TemperatureUnit.values
        .flatMap(_.symbols)
        .filter(s => s.startsWith("°") && s.length > 1)
        .map(_.substring(1))
      __Verify__
      sut should containTheSameElementsAs(expected)
    }
  }

  "Predefined temperature units" - {

    "3.0 <<temperature unit>> should be converted to the equivalent value in Kelvin" in {
      __Exercise__
      val conversions =
        Table(
          ("temperatures", "expected"),
          (Seq(3.0.yK, 3.0 yK, 3.0 (yK)), 3e-24),
          (Seq(3.0.zK, 3.0 zK, 3.0 (zK)), 3e-21),
          (Seq(3.0.aK, 3.0 aK, 3.0 (aK)), 3e-18),
          (Seq(3.0.fK, 3.0 fK, 3.0 (fK)), 3e-15),
          (Seq(3.0.pK, 3.0 pK, 3.0 (pK)), 3e-12),
          (Seq(3.0.nK, 3.0 nK, 3.0 (nK)), 3e-9),
          (Seq(3.0.μK, 3.0 μK, 3.0 (μK)), 3e-6),
          (Seq(3.0.mK, 3.0 mK, 3.0 (mK)), 3e-3),
          (Seq(3.0.cK, 3.0 cK, 3.0 (cK)), 3e-2),
          (Seq(3.0.dK, 3.0 dK, 3.0 (dK)), 3e-1),
          (Seq(3.0.K, 3.0 K, 3.0 (K)), 3.0),
          (Seq(3.0.daK, 3.0 daK, 3.0 (daK)), 3e1),
          (Seq(3.0.hK, 3.0 hK, 3.0 (hK)), 3e2),
          (Seq(3.0.kK, 3.0 kK, 3.0 (kK)), 3e3),
          (Seq(3.0.MK, 3.0 MK, 3.0 (MK)), 3e6),
          (Seq(3.0.GK, 3.0 GK, 3.0 (GK)), 3e9),
          (Seq(3.0.TK, 3.0 TK, 3.0 (TK)), 3e12),
          (Seq(3.0.PK, 3.0 PK, 3.0 (PK)), 3e15),
          (Seq(3.0.EK, 3.0 EK, 3.0 (EK)), 3e18),
          (Seq(3.0.ZK, 3.0 ZK, 3.0 (ZK)), 3e21),
          (Seq(3.0.YK, 3.0 YK, 3.0 (YK)), 3e24),

          (Seq(0.0.degC, 0.0 degC, 0.0 (degC)), 273.15),
          (Seq(3.0.degC, 3.0 degC, 3.0 (degC)), 276.15),
          (Seq(0.0.℃  , 0.0 ℃   , 0.0 (℃)) , 273.15),
          (Seq(3.0.℃  , 3.0 ℃   , 3.0 (℃)) , 276.15),
          (Seq(0.0.°C  ,           0.0 (°C)) , 273.15),
          (Seq(3.0.°C  ,           3.0 (°C)) , 276.15),

          (Seq(0.0.degF, 0.0 degF, 0.0 (degF)), 273.15 - 32.0 * 5.0 / 9.0),
          (Seq(3.0.degF, 3.0 degF, 3.0 (degF)), 273.15 - 29.0 * 5.0 / 9.0),
          (Seq(0.0.℉   , 0.0 ℉   , 0.0 (℉))   , 273.15 - 32.0 * 5.0 / 9.0),
          (Seq(3.0.℉   , 3.0 ℉   , 3.0 (℉))   , 273.15 - 29.0 * 5.0 / 9.0),
          (Seq(0.0.°F  ,           0.0 (°F))  , 273.15 - 32.0 * 5.0 / 9.0),
          (Seq(3.0.°F  ,           3.0 (°F))  , 273.15 - 29.0 * 5.0 / 9.0),

          (Seq(0.0.degDe, 0.0 degDe, 0.0 (degDe)), 373.15),
          (Seq(3.0.degDe, 3.0 degDe, 3.0 (degDe)), 371.15),
          (Seq(0.0.°De  ,           0.0 (°De)) , 373.15),
          (Seq(3.0.°De  ,           3.0 (°De)) , 371.15),

          (Seq(0.0.degN, 0.0 degN, 0.0 (degN)), 273.15),
          (Seq(3.0.degN, 3.0 degN, 3.0 (degN)), 273.15 + 100.0/11.0),
          (Seq(0.0.°N  ,           0.0 (°N)) , 273.15),
          (Seq(3.0.°N  ,           3.0 (°N)) , 273.15 + 100.0/11.0),

          (Seq(0.0.degR, 0.0 degR, 0.0 (degR)), 0.0),
          (Seq(3.0.degR, 3.0 degR, 3.0 (degR)), 5.0 / 3.0),
          (Seq(0.0.°R  ,           0.0 (°R))  , 0.0),
          (Seq(3.0.°R  ,           3.0 (°R))  , 5.0 / 3.0),

          (Seq(0.0.degRe, 0.0 degRe, 0.0 (degRe)), 273.15),
          (Seq(3.0.degRe, 3.0 degRe, 3.0 (degRe)), 273.15 + 15.0 / 4.0),
          (Seq(0.0.°Re  ,            0.0 (°Re))  , 273.15),
          (Seq(3.0.°Re  ,            3.0 (°Re))  , 273.15 + 15.0 / 4.0),

          (Seq(0.0.degRo, 0.0 degRo, 0.0 (degRo)), 273.15 - 7.5 * 40.0 / 21.0),
          (Seq(3.0.degRo, 3.0 degRo, 3.0 (degRo)), 273.15 - 4.5 * 40.0 / 21.0),
          (Seq(0.0.°Ro  ,            0.0 (°Ro))  , 273.15 - 7.5 * 40.0 / 21.0),
          (Seq(3.0.°Ro  ,            3.0 (°Ro))  , 273.15 - 4.5 * 40.0 / 21.0),

          (Seq(0.0.GM, 0.0 GM, 0.0 (GM)), 422.038),
          (Seq(3.0.GM, 3.0 GM, 3.0 (GM)), 422.038 + 125.0 / 3.0)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Temperature[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut K) should equal(%%%%(expected))
        }
      }
    }

    "3.0 K should be converted to the equivalent value in other temperature units" in {
      __SetUp__
      val q0 = 0.0 (K)
      val q  = 3.0 (K)
      __Exercise__
      val conversions =
        Table(
          ("temperatures", "expected"),
          (Seq(q.yK, q yK, q(yK)), 3e24),
          (Seq(q.zK, q zK, q(zK)), 3e21),
          (Seq(q.aK, q aK, q(aK)), 3e18),
          (Seq(q.fK, q fK, q(fK)), 3e15),
          (Seq(q.pK, q pK, q(pK)), 3e12),
          (Seq(q.nK, q nK, q(nK)), 3e9),
          (Seq(q.μK, q μK, q(μK)), 3e6),
          (Seq(q.mK, q mK, q(mK)), 3e3),
          (Seq(q.cK, q cK, q(cK)), 3e2),
          (Seq(q.dK, q dK, q(dK)), 3e1),
          (Seq(q.K, q K, q(K)), 3.0),
          (Seq(q.daK, q daK, q(daK)), 3e-1),
          (Seq(q.hK, q hK, q(hK)), 3e-2),
          (Seq(q.kK, q kK, q(kK)), 3e-3),
          (Seq(q.MK, q MK, q(MK)), 3e-6),
          (Seq(q.GK, q GK, q(GK)), 3e-9),
          (Seq(q.TK, q TK, q(TK)), 3e-12),
          (Seq(q.PK, q PK, q(PK)), 3e-15),
          (Seq(q.EK, q EK, q(EK)), 3e-18),
          (Seq(q.ZK, q ZK, q(ZK)), 3e-21),
          (Seq(q.YK, q YK, q(YK)), 3e-24),

          (Seq(q0.degC, q0 degC, q0(degC)), -273.15),
          (Seq(q.degC , q degC , q(degC)) , -270.15),
          (Seq(q0.℃  , q0 ℃  , q0(℃))   , -273.15),
          (Seq(q.℃   , q ℃   , q(℃))    , -270.15),
          (Seq(q0.°C  ,         q0(°C))   , -273.15),
          (Seq(q.°C   ,         q(°C))    , -270.15),

          (Seq(q0.degF, q0 degF, q0(degF)), 32.0 - 273.15 * 9.0 / 5.0),
          (Seq(q.degF , q degF , q(degF)) , 32.0 - 270.15 * 9.0 / 5.0),
          (Seq(q0.℉   , q0 ℉   , q0(℉))   , 32.0 - 273.15 * 9.0 / 5.0),
          (Seq(q.℉    , q ℉    , q(℉))    , 32.0 - 270.15 * 9.0 / 5.0),
          (Seq(q0.°F  ,          q0(°F))  , 32.0 - 273.15 * 9.0 / 5.0),
          (Seq(q.°F   ,          q(°F))   , 32.0 - 270.15 * 9.0 / 5.0)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Celsius and Fahrenheit" - {

    "°C -> °F" - {
      val conversions =
        Table(
          ("Celsius", "Fahrenheit"),
          (0.0      , 32.0),
          (100.0    , 212.0)
        )
      __Verify__
      forAll(conversions) { (c: Double, f: Double) =>
        c.degC.degF should equal (%%%%(f))
      }
    }

    "°F -> °C" - {
      val conversions =
        Table(
          ("Fahrenheit", "Celsius"),
          (32.0        , 0.0),
          (212.0       , 100.0)
        )
      __Verify__
      forAll(conversions) { (f: Double, c: Double) =>
        f.degF.degC should equal (%%%%(c))
      }
    }
  }
}
