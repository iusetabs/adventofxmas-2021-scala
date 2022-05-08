package third

import helpers.CommonHelper

import scala.annotation.tailrec
import scala.io.Source

object BinaryCounter extends App {

  lazy val SAMPLE_SOURCE_CONFIG_KEY: String = "thirdday.sample.source.path"
  lazy val SOURCE_CONFIG_KEY: String = "thirdday.source.path"

  case class BinaryBit(isZero: Boolean = false, isOne: Boolean = false) {
    def bitMatches(bit: String): Boolean = {
      isZero && bit == "0" || isOne && bit == "1"
    }

    override def toString: String = {
      Option.when(isOne)("1").getOrElse("0")
    }
  }

  def inputValues(inputFilePath: String = SOURCE_CONFIG_KEY): List[String] = {
    val inputFile = CommonHelper.getStringFromConf(inputFilePath)
    val reader = Source.fromFile(inputFile)
    val values = reader.getLines.toList
    reader.close()
    values
  }

  def countBits(binary: String): Map[Int, BinaryBit] = {
    binary.toList.zipWithIndex.map {
      case (char, pos) =>
        val binaryBit = Option.when(char=='0')(BinaryBit(isZero = true)).getOrElse(BinaryBit(isOne = true))
        (pos, binaryBit)
    }.toMap
  }

  @tailrec
  def getMostCommonBits(values: List[String], listOfBinaryBits: List[Map[Int, BinaryBit]]): List[Map[Int, BinaryBit]] = {
    if (values.isEmpty) {
      listOfBinaryBits
    } else {
      values match {
        case binary::tail =>
          val bitsAsMap = countBits(binary)
          getMostCommonBits(tail, listOfBinaryBits++List(bitsAsMap))
        case _ => ???
      }
    }
  }

  def getBitsSortedByPosition(bitsWithPos: List[Map[Int, BinaryCounter.BinaryBit]]): List[(Int, List[BinaryBit])] = {
    bitsWithPos.flatMap {
      mapWithPosAndBit =>
        mapWithPosAndBit.map {
          case (pos, bit) =>
            (pos, bit)
        }
    }.groupBy {
      case (pos, _) =>
        pos
    }.toList.map {
      case (pos, posWithBit) =>
        val bits = posWithBit.map {
          case (_, bit) => bit
        }
        (pos, bits)
    }.sortBy {
      case (pos, _) =>
        pos
    }
  }

  def generateGammaAndEpsilon(bitsWithPos: List[Map[Int, BinaryBit]]): (String, String) = {
    val bitsSortedByPos = getBitsSortedByPosition(bitsWithPos)
    val gamma = bitsSortedByPos.map {
      case (_, bits) =>
        val ones = bits.count(_.isOne)
        val zeros = bits.count(_.isZero)
        Option.when(ones > zeros)("1").getOrElse("0")
    }.mkString
    val epsilon = bitsSortedByPos.map {
      case (_, bits) =>
        val ones = bits.count(_.isOne)
        val zeros = bits.count(_.isZero)
        Option.when(ones < zeros)("1").getOrElse("0")
    }.mkString
    (gamma, epsilon)
  }

  //To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position,
  // and keep only numbers with that bit in that position.
  // If 0 and 1 are equally common, keep values with a 1 in the position being considered.
  @tailrec
  def generateOxygen(bitsWithPos: List[(Int, List[BinaryBit])], pos: Int = 0): String = {
    val onlyOneBitLeft = bitsWithPos.forall {
      case (_, bits) =>
        bits.length == 1
    }

    if (onlyOneBitLeft) {
      val returnString = bitsWithPos.flatMap {
        case (_, bits) =>
          bits.map(_.toString)
      }.mkString
      returnString
    } else {
      val bitsAtPos = bitsWithPos.filter {
        case (xPos, _) =>
          xPos == pos
      }.flatMap {
        case (_, bits) =>
          bits
      }
      val ones = bitsAtPos.count(_.isOne)
      val zeros = bitsAtPos.count(_.isZero)
      val mostCommonBit = Option.when(ones >= zeros)("1").getOrElse("0")
      val filteredBitsAtPosWithIndx = bitsAtPos.zipWithIndex.filter {
        case (bit, _) =>
          bit.bitMatches(mostCommonBit)
      }
      val filteredBitsAtPos = filteredBitsAtPosWithIndx.map {
        case (bit, _) =>
          bit
      }
      val acceptedIndexes = filteredBitsAtPosWithIndx.map {
        case (_, indx) =>
          indx
      }
      val filteredBitsAtAllOtherPos = (bitsWithPos.filter {
        case (xPos, _) =>
          xPos != pos
      }).map {
        case (xPos, bits) =>
          val bitsAtAcceptedIndexes = bits.zipWithIndex.filter {
            case (_, indx) =>
              acceptedIndexes.contains(indx)
          }.map {
            case (bit, _) =>
              bit
          }
          (xPos, bitsAtAcceptedIndexes)
      }

      val newBitsWithPos = (filteredBitsAtAllOtherPos ++ List((pos, filteredBitsAtPos))).sortBy{
        case (pos, _) =>
          pos
      }
      generateOxygen(newBitsWithPos, pos+1)
    }
  }

  //To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position,
  // and keep only numbers with that bit in that position.
  // If 0 and 1 are equally common, keep values with a 1 in the position being considered.
  @tailrec
  def generateCarbon(bitsWithPos: List[(Int, List[BinaryBit])], pos: Int = 0): String = {
    val onlyOneBitLeft = bitsWithPos.forall {
      case (_, bits) =>
        bits.length == 1
    }

    if (onlyOneBitLeft) {
      val returnString = bitsWithPos.flatMap {
        case (_, bits) =>
          bits.map(_.toString)
      }.mkString
      returnString
    } else {
      val bitsAtPos = bitsWithPos.filter {
        case (xPos, _) =>
          xPos == pos
      }.flatMap {
        case (_, bits) =>
          bits
      }
      val ones = bitsAtPos.count(_.isOne)
      val zeros = bitsAtPos.count(_.isZero)
      val mostCommonBit = Option.when(ones < zeros)("1").getOrElse("0")
      val filteredBitsAtPosWithIndx = bitsAtPos.zipWithIndex.filter {
        case (bit, _) =>
          bit.bitMatches(mostCommonBit)
      }
      val filteredBitsAtPos = filteredBitsAtPosWithIndx.map {
        case (bit, _) =>
          bit
      }
      val acceptedIndexes = filteredBitsAtPosWithIndx.map {
        case (_, indx) =>
          indx
      }
      val filteredBitsAtAllOtherPos = (bitsWithPos.filter {
        case (xPos, _) =>
          xPos != pos
      }).map {
        case (xPos, bits) =>
          val bitsAtAcceptedIndexes = bits.zipWithIndex.filter {
            case (_, indx) =>
              acceptedIndexes.contains(indx)
          }.map {
            case (bit, _) =>
              bit
          }
          (xPos, bitsAtAcceptedIndexes)
      }

      val newBitsWithPos = (filteredBitsAtAllOtherPos ++ List((pos, filteredBitsAtPos))).sortBy{
        case (pos, _) =>
          pos
      }
      generateCarbon(newBitsWithPos, pos+1)
    }
  }


  def partOne(inputFileConfKey: String = SOURCE_CONFIG_KEY): Long = {
    val values = inputValues(inputFileConfKey)
    val bitsWithPos = getMostCommonBits(values, List.empty)
    val (gamma, epsilon) = generateGammaAndEpsilon(bitsWithPos)
    val gammaAsDecimal = Integer.parseInt(gamma, 2)
    val epsilonAsDecimal = Integer.parseInt(epsilon, 2)
    gammaAsDecimal * epsilonAsDecimal
  }

  def partTwo(inputFileConfKey: String = SOURCE_CONFIG_KEY): Long = {
    val values = inputValues(inputFileConfKey)
    val bitsWithPos = getMostCommonBits(values, List.empty)
    val bitsSortedByPos = getBitsSortedByPosition(bitsWithPos)
    val oxygen = generateOxygen(bitsSortedByPos)
    val oxygenAsDecimal = Integer.parseInt(oxygen, 2)
    val carbon = generateCarbon(bitsSortedByPos)
    val carbonAsDecimal = Integer.parseInt(carbon, 2)
    oxygenAsDecimal * carbonAsDecimal
  }

}
