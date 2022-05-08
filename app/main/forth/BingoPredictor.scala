package forth

import helpers.CommonHelper
import scala.io.Source

object BingoPredictor extends App {

  /*
  22 13 17 11  0
  8  2 23  4 24
  21  9 14 16  7
  6 10  3 18  5
  1 12 20 15 19
  */

  lazy val SAMPLE_SOURCE_CONFIG_KEY: String = "forthday.sample.source.path"
  lazy val SOURCE_CONFIG_KEY: String = "forthday.source.path"

  def inputValues(inputFilePath: String = SOURCE_CONFIG_KEY): List[List[Int]] = {
    val inputFile = CommonHelper.getStringFromConf(inputFilePath)
    val reader = Source.fromFile(inputFile)
    val values = reader.getLines.toList
    reader.close()
    values.map {
      line =>
        line.split(" ").flatMap {
          number =>
            number.toIntOption
        }.toList
    }
  }

  case class BingoRowEntry(value: Int, marked: Boolean = false)

  case class BingoRow(values: List[BingoRowEntry]) {
    def valuesAsString: String = {
      values.map {
        rowEntry =>
          rowEntry.value.toString
      }.mkString(" ")
    }
  }

  case class BingoTable(rows: List[BingoRow]) {
    def addRow(newRow: BingoRow): BingoTable = {
      copy(rows :+ newRow)
    }

    override def toString: String = {
      rows.map {
        row =>
          row.valuesAsString
      }.mkString("\n")
    }
  }

  object BingoTable {
    def empty: BingoTable = {
      BingoTable(List.empty)
    }

    def createBingoTable(input: List[List[Int]]): BingoTable = {
      BingoTable(
        rows =
          input.map {
            rowAsList =>
              BingoRow(
                values = rowAsList.map {
                  value =>
                    BingoRowEntry(value)
                }
              )
          }
      )
    }
  }

  def partOne(): Unit = {
    val values = inputValues(SAMPLE_SOURCE_CONFIG_KEY)
    val bingoTable = BingoTable.createBingoTable(values)
    print(bingoTable.toString())
  }

  print("running part one")
  partOne()

}