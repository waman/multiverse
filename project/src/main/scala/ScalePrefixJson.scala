import java.io.File

import GenerationUtil.utf8
import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class ScalePrefix(name: String, prefix: String, aliases: Array[String], scale: String){
  lazy val _aliases: Seq[String] =
    if (aliases != null) aliases else Nil
}

class ScalePrefixJson(jsonFile: File) extends JsonResource(jsonFile){

  private val scalePrefixType: Class[_ >: Array[ScalePrefix]] = new TypeToken[Array[ScalePrefix]]() {}.getRawType

  val scalePrefixes: Seq[ScalePrefix] = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, scalePrefixType).asInstanceOf[Array[ScalePrefix]].toSeq
  }

  override def isGenerating: Boolean = false
  override def isUnitDefinitionJson: Boolean = false
}