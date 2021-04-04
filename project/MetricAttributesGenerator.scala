import sbt.io.IO

import java.io.File

object MetricAttributesGenerator {

  import GenerationUtil._

  private val metricUnitsNames: Seq[String] = Seq("Length", "Area", "Volume")

  def generate(destRoot: File, jsons: JsonResources): File = {
    val destFile = IO.resolve(destRoot, new File("unit/defs/MetricAttributes.scala"))

    IO.writer(destFile, "", utf8, append = false) { writer =>
      writer.write(s"""package $rootPackage.unit.defs\n\n""")

      val metricUnitdefs = jsons.unitdefs.filter(ud => metricUnitsNames.contains(ud.id))
                                      .map(ud => ud.asInstanceOf[LinearUnitdefJson])

      metricUnitdefs.flatMap(ud => ud.unitCategory.attributes.flatMap(a => a.parents))
          .distinct.foreach{ u =>
        writer.write(s"""sealed trait ${toObjectName(u)}Attribute\n""")
      }

      //    object MetricAttributes{
      //      final object MoKα1 extends xunitAttribute
      //      final object Adm extends nautical_mileAttribute
      //      ...
      //    }
      writer.write(
        """
          |object MetricAttributes{
          |""".stripMargin)

      val attMap: Map[String, Seq[String]] = metricUnitdefs.flatMap(ud => ud.unitCategory._attributes)
                          .groupBy(_.name).mapValues(atts => atts.flatMap(_.parents))

      attMap.foreach{ case (name, parents) =>
        val traits = parents.map(toObjectName(_) + "Attribute").mkString(" with ")
        writer.write(s"""  final object $name extends $traits\n""")
      }

      writer.write("}")
    }
    destFile
  }
}
