package helpers

import java.io.File
import java.nio.file.Paths
import java.util.Properties
import scala.io.{BufferedSource, Source}

object CommonHelper {

  val confFile: File = Paths.get("C:\\Users\\james.nolan\\Development\\AdventOfXmasScala\\conf\\common.properties").toFile
  val properties: Properties = new Properties()
  val source: BufferedSource = Source.fromFile(confFile)
  properties.load(source.bufferedReader())

  def getStringFromConf(key: String): String = {
    val res = properties.getProperty(key)
    res
  }

}