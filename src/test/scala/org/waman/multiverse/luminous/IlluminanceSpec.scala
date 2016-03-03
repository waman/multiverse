package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class IlluminanceSpec
  extends AbstractQuantityAndUnitSpec[IlluminanceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[IlluminanceUnit]

  "3.0 <<illuminance unit>> should be converted to the equivalent value in Lux" in {
    __Exercise__
    val conversions =
      Table(
        ("illuminances", "expected"),
        (Seq(3.0.ylx, 3.0 ylx, 3.0 (ylx)), 3e-24),
        (Seq(3.0.zlx, 3.0 zlx, 3.0 (zlx)), 3e-21),
        (Seq(3.0.alx, 3.0 alx, 3.0 (alx)), 3e-18),
        (Seq(3.0.flx, 3.0 flx, 3.0 (flx)), 3e-15),
        (Seq(3.0.plx, 3.0 plx, 3.0 (plx)), 3e-12),
        (Seq(3.0.nlx, 3.0 nlx, 3.0 (nlx)), 3e-9),
        (Seq(3.0.microLux, 3.0 microLux, 3.0 (microLux)), 3e-6),
        (Seq(3.0.microLx, 3.0 microLx, 3.0 (microLx)), 3e-6),
        (Seq(3.0.μlx, 3.0 μlx, 3.0 (μlx)), 3e-6),
        (Seq(3.0.mlx, 3.0 mlx, 3.0 (mlx)), 3e-3),
        (Seq(3.0.clx, 3.0 clx, 3.0 (clx)), 3e-2),
        (Seq(3.0.dlx, 3.0 dlx, 3.0 (dlx)), 3e-1),
        (Seq(3.0.lx , 3.0 lx , 3.0 (lx)) , 3.0),
        (Seq(3.0.dalx, 3.0 dalx, 3.0 (dalx)), 3e1),
        (Seq(3.0.hlx, 3.0 hlx, 3.0 (hlx)), 3e2),
        (Seq(3.0.klx, 3.0 klx, 3.0 (klx)), 3e3),
        (Seq(3.0.Mlx, 3.0 Mlx, 3.0 (Mlx)), 3e6),
        (Seq(3.0.Glx, 3.0 Glx, 3.0 (Glx)), 3e9),
        (Seq(3.0.Tlx, 3.0 Tlx, 3.0 (Tlx)), 3e12),
        (Seq(3.0.Plx, 3.0 Plx, 3.0 (Plx)), 3e15),
        (Seq(3.0.Elx, 3.0 Elx, 3.0 (Elx)), 3e18),
        (Seq(3.0.Zlx, 3.0 Zlx, 3.0 (Zlx)), 3e21),
        (Seq(3.0.Ylx, 3.0 Ylx, 3.0 (Ylx)), 3e24),

        (Seq(3.0.ph, 3.0 ph, 3.0 (ph)), 3e4),
        (Seq(3.0.fc, 3.0 fc, 3.0 (fc)), 3.0 * 10.763910417)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Illuminance[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut lx) should equal (%%%%(expected))
      }
    }
  }

  "3.0 lx should be converted to the equivalent value in other illuminance units" in {
    __SetUp__
    val q = 3.0 (lx)
    __Exercise__
    val conversions =
      Table(
        ("illuminances", "expected"),
        (Seq(q.ylx, q ylx, q (ylx)), 3e24),
        (Seq(q.zlx, q zlx, q (zlx)), 3e21),
        (Seq(q.alx, q alx, q (alx)), 3e18),
        (Seq(q.flx, q flx, q (flx)), 3e15),
        (Seq(q.plx, q plx, q (plx)), 3e12),
        (Seq(q.nlx, q nlx, q (nlx)), 3e9),
        (Seq(q.μlx, q μlx, q (μlx)), 3e6),
        (Seq(q.microLux, q microLux, q (microLux)), 3e6),
        (Seq(q.microLx, q microLx, q (microLx)), 3e6),
        (Seq(q.mlx, q mlx, q (mlx)), 3e3),
        (Seq(q.clx, q clx, q (clx)), 3e2),
        (Seq(q.dlx, q dlx, q (dlx)), 3e1),
        (Seq(q.lx , q lx , q (lx)) , 3.0),
        (Seq(q.dalx, q dalx, q (dalx)), 3e-1),
        (Seq(q.hlx, q hlx, q (hlx)), 3e-2),
        (Seq(q.klx, q klx, q (klx)), 3e-3),
        (Seq(q.Mlx, q Mlx, q (Mlx)), 3e-6),
        (Seq(q.Glx, q Glx, q (Glx)), 3e-9),
        (Seq(q.Tlx, q Tlx, q (Tlx)), 3e-12),
        (Seq(q.Plx, q Plx, q (Plx)), 3e-15),
        (Seq(q.Elx, q Elx, q (Elx)), 3e-18),
        (Seq(q.Zlx, q Zlx, q (Zlx)), 3e-21),
        (Seq(q.Ylx, q Ylx, q (Ylx)), 3e-24),

        (Seq(q.ph, q ph, q (ph)), 3e-4),
        (Seq(q.fc, q fc, q (fc)), 3.0 / 10.763910417)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }

  "Quotient illuminance unit" - {

    "Illuminance unit of lm/in2 should equal 1550.0031 lx" in {
      __Exercise__
      val sut = lm/in2
      __Verify__
      sut.unitInLux.toDouble should equal (%%%%(1550.0031))
    }

    "3.0 lm/in2 should equal 3.0 * 1550.0031 lx" in {
      __Exercise__
      val conversions =
        Table(
          ("illuminance", "expected"),
          (3.0.lm/in2, 3.0 * 1550.0031),
          (3.0 lm/in2, 3.0 * 1550.0031),
          (3.0 (lm/in2), 3.0 * 1550.0031)
        )
      __Verify__
      forAll(conversions){ (sut: Illuminance[Double], expected: Double) =>
        sut.lx should equal (%%%%(expected))
      }
    }

    "3.0 lx should equal 3.0 / 1550.0031 lm/in2" in {
      __SetUp__
      val q = 3.0 (lx)
      val expected = 3.0 / 1550.0031
      __Exercise__
      val conversions =
        Table(
          ("illuminance", "expected"),
          (q.lm/in2, expected),
          (q lm/in2, expected),
          (q (lm/in2), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
