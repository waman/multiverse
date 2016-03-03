package org.waman.multiverse

import java.lang.reflect.Method

abstract class AbstractQuantityAndUnitSpec[U <: PhysicalUnit[U]] extends MultiverseCustomSpec{

  protected val getUnitClass: Class[U]

  protected lazy val getUnitClassName = getUnitClass.getName
  protected lazy val getQuantityClassName = getUnitClassName.substring(0, getUnitClassName.length - 4)

  protected lazy val getPostfixOpsTrait: Option[Class[_]] =
    try Some(Class.forName(getQuantityClassName + "PostfixOps"))
    catch { case _: ClassNotFoundException => None }

  protected lazy val getPostfixOpsObjectClass: Class[_] =
    Class.forName(getQuantityClassName + "PostfixOps$")

  protected lazy val getPostfixOpsObject: Any =
    getPostfixOpsObjectClass.getField("MODULE$").get(null)

  protected lazy val getDotTrait: Option[Class[_]] =
    try Some(Class.forName(getQuantityClassName + "Dot"))
    catch { case _: ClassNotFoundException => None }

  protected lazy val getPerTrait: Option[Class[_]] =
    try Some(Class.forName(getQuantityClassName + "Per"))
    catch { case _: ClassNotFoundException => None }

  protected lazy val getConstantsDefined: Option[ConstantsDefined[U]] =
    try{
      Some(
        Class.forName(getUnitClassName + "$").getField("MODULE$")
          .get(null).asInstanceOf[ConstantsDefined[U]])
    }catch{ case _: Exception => None }

  "Implementation Constraints" - {

    def getConstantNames: Seq[String] =
      getUnitClass.getClasses
        .map(_.getSimpleName)
        .filterNot(name =>
          name.startsWith("Intrinsic")
          || name.startsWith("Product")
          || name.startsWith("Quotient")
          || name.startsWith("LengthOver")
          || name.startsWith("VelocityOver"))
        .map(s => s.substring(0, s.length - 1))

    "XxxUnit.values property should return all predefined values of the unit" in {
      getConstantsDefined match { case Some(constDef) =>
        __SetUp__
        val expected = getConstantNames
        __Exercise__
        val sut = constDef.values.map(_.name)
        __Verify__
        sut should containTheSameElementsAs(expected)

      case None => cancel("No constant is defined") }
    }

    "Unit object's 'name' property should have the same value as its name " in {
      getConstantsDefined match { case Some(constDef) =>
        __SetUp__
        val expected = getConstantNames
        __Exercise__
        val suts = constDef.values
        __Verify__
        suts.foreach{ sut =>
          val expected = truncateLast(sut.getClass.getSimpleName)
          sut.name should equal (expected)
        }

      case None => cancel("No constant is defined") }
    }

    """XxxPostfixOps trait should have properties
      | whose names are the same as 'symbol' properties of unit objects""".stripMargin in {

      getPostfixOpsTrait match { case Some(postfixOps) =>
        getConstantsDefined match { case Some(constDef) =>

          def getPostfixOpsPropertyNames: Seq[String] =
            postfixOps.getMethods
              .filterNot(_.getName.endsWith("PostfixOps"))
              .flatMap(methodToSymbols)

          def methodToSymbols(m: Method): Seq[String] = m.getParameterCount match {
            case 0 => Seq(decodePropertyName(m.getName))
            case 1 =>
              val name = m.getName
              val pf = getPostfixOpsObjectClass
                         .getMethod("_" + name).invoke(getPostfixOpsObject)
                         .asInstanceOf[PartialFunction[Context, _]]
              Context.values
                .filter(pf.isDefinedAt)
                .map(decodePropertyName(name) + "(" + _.symbol + ")")
          }

          __SetUp__
          val sut = getPostfixOpsPropertyNames
          val expected = constDef.values
                           .flatMap(_.symbols)
                           .filterNot(s => s.startsWith("°") && s.length > 1)
                             // remove degree temperature like °C (test in another testcase)
          __Verify__
          sut should containTheSameElementsAs(expected)

        case None => cancel("XxxUnit object is not defined") }
      case None => cancel("XxxPostfixOps trait is not defined") }
    }

    "XxxDot trait should have methods whose names are 'symbol' properties of unit objects" in {
      getDotTrait match { case Some(dot) =>
        getConstantsDefined match { case Some(constDef) =>
          __SetUp__
          val sut = getPropertyNames(dot, "Dot")
          val expected =
            constDef.values
              .flatMap(_.symbols)
              .filterNot(_.contains("("))
          __Verify__
          sut should containTheSameElementsAs(expected)

        case None => cancel("XxxUnit object is not defined") }
      case None => cancel("XxxDot trait is not defined") }
    }

    "XxxPer trait should have methods whose names are 'symbol' properties of unit objects" in {
      getPerTrait match { case Some(per) =>
        getConstantsDefined match { case Some(constDef) =>
          __SetUp__
          val sut = getPropertyNames(per, "Per")
          val expected =
            constDef.values
              .flatMap(_.symbols)
              .filterNot(_.contains("("))
          __Verify__
          sut should containTheSameElementsAs(expected)

        case None => cancel("XxxUnit object is not defined") }
      case None => cancel("XxxPer trait is not defined") }
    }
  }

  private def getPropertyNames(c: Class[_], ignoreMethodPostfix: String): Seq[String] =
    c.getMethods
      .filterNot(_.getName.endsWith(ignoreMethodPostfix))
      .map(_.getName)
      .map(decodePropertyName)

  private def decodePropertyName(name: String): String =
    if(name.startsWith("$"))
      name.split('$')
        .filterNot(_.length == 0)
        .map(_.substring(1))
        .map(Integer.valueOf(_, 16))
        .flatMap(Character.toChars(_)).mkString("")
    else
      name
}
