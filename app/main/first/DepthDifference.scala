package first

import helpers.CommonHelper
import scala.annotation.tailrec
import scala.io.Source

object DepthDifference extends App {

  lazy val SAMPLE_SOURCE_CONFIG_KEY: String = "firstday.sample.source.path"
  lazy val SOURCE_CONFIG_KEY: String = "firstday.source.path"

  def inputValues(inputFilePath: String = SOURCE_CONFIG_KEY): List[Long] = {
    val inputFile = CommonHelper.getStringFromConf(inputFilePath)
    val reader = Source.fromFile(inputFile)
    val values = reader.getLines.flatMap(_.toLongOption).toList
    reader.close()
    values
  }

  @tailrec
  def countIncreases(values: List[Long], count: Long = 0): Long = {
    if (values.isEmpty) {
      count
    } else {
      values match {
        case depthA::depthB::tail =>
          val newCount = Option.when(depthA < depthB)(count+1).getOrElse(count)
          countIncreases(depthB+:tail, newCount)
        case _::tail =>
          countIncreases(tail, count)
        case _ => ???
      }
    }
  }

  @tailrec
  def getListOfTriplets(values: List[Long], returnList: List[(Long, Long, Long)] = List.empty) : List[(Long, Long, Long)] = {
    if (values.isEmpty) {
      returnList
    } else {
      values match {
        case depthA::depthB::depthC::tail =>
          getListOfTriplets(List(depthB, depthC)++tail, returnList:+(depthA, depthB, depthC))
        case _ if values.length < 3 => getListOfTriplets(List.empty, returnList)
        case _ => ???
      }
    }
  }

  def partOne(inputFileConfKey: String = SOURCE_CONFIG_KEY): Long = {
    val values = inputValues(inputFileConfKey)
    countIncreases(values)
  }

  def partTwo(inputFileConfKey: String = SOURCE_CONFIG_KEY): Long = {
    val values = inputValues(inputFileConfKey)
    val listOfSums = getListOfTriplets(values).map {
      case (numA, numB, numC) =>
        numA + numB + numC
      case _ => ???
    }
    countIncreases(listOfSums)
  }
}
