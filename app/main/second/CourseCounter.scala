package second

import helpers.CommonHelper
import scala.annotation.tailrec
import scala.io.Source

object CourseCounter extends App {

  lazy val SAMPLE_SOURCE_CONFIG_KEY: String = "firstday.sample.source.path"
  lazy val SOURCE_CONFIG_KEY: String = "firstday.source.path"

  case class Course(horizontalPos: Long, depth: Long, aim: Long = 0) {

    def forward(num: Long): Course = {
      if ( aim == 0 )
        Course(horizontalPos + num, depth, aim)
      else {
        Course(horizontalPos + num, depth + (num * aim), aim)
      }
    }

    def down(num: Long): Course = {
      Course(horizontalPos, depth, aim + num)
    }

    def up(num: Long): Course = {
      Course(horizontalPos, depth, aim - num)
    }

    def partOneCalc(): Long = {
      horizontalPos * depth
    }
  }

  def inputValues(inputFilePath: String = SOURCE_CONFIG_KEY): List[String] = {
    val inputFile = CommonHelper.getStringFromConf(inputFilePath)
    val reader = Source.fromFile(inputFile)
    val values = reader.getLines.toList
    reader.close()
    values
  }

  @tailrec
  def getFinalCourseFromDirections(directions: List[String], finalCourse: Course = Course(0, 0)): Course = {
    if (directions.isEmpty) {
      finalCourse
    } else {
      directions match {
        case course::tail =>
          val instructionSplit = course.trim().split(" ")
          val (instruction, num) = (instructionSplit.headOption, instructionSplit.lastOption)
          (instruction, num) match {
            case (Some(forward), Some(x)) if forward == "forward" =>
              getFinalCourseFromDirections(tail, finalCourse.forward(x.toLongOption.getOrElse(0)))
            case (Some(down), Some(x)) if down == "down" =>
              getFinalCourseFromDirections(tail, finalCourse.down(x.toLongOption.getOrElse(0)))
            case (Some(up), Some(x)) if up == "up" =>
              getFinalCourseFromDirections(tail, finalCourse.up(x.toLongOption.getOrElse(0)))
            case _ => ???
          }
      }
    }
  }

  def partOne(inputFileConfKey: String = SOURCE_CONFIG_KEY): Long = {
    val values = inputValues(inputFileConfKey)
    val res = getFinalCourseFromDirections(values)
    res.partOneCalc()
  }

  /*def partTwo(inputFileConfKey: String = SOURCE_CONFIG_KEY): Long = {
  }*/
}
