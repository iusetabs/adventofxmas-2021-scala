package first

import org.scalatest.FunSuite

/** Tests around Depth Difference */
class Day1Suite extends FunSuite {

  // PART ONE

  test("There should be only two increases detected") {
    assert(DepthDifference.countIncreases(List(1, 2, 1, 2)) == 2)
  }

  test("There should be no increases detected") {
    assert(DepthDifference.countIncreases(List(0,0,0)) == 0)
  }

  test("There should be no increases detected for an empty list"){
    assert(DepthDifference.countIncreases(List.empty) == 0)
  }

  test("Using the sample data, there should be 7 increases detected") {
    assert(DepthDifference.partOne("firstday.sample.source.path") == 7)
  }

  test("Using the test data, there should be 1696 increases detected") {
    assert(DepthDifference.partOne("firstday.source.path") == 1696)
  }

  // PART TWO

  test("Given simple list, should return correct list") {
    val expectedRes = List((1, 2, 3), (2, 3, 4), ( 3, 4, 5))
    val testList : List[Long] = List(1, 2, 3, 4, 5)
    assert(DepthDifference.getListOfTriplets(testList) == expectedRes)
  }

  test("Given sample data, there should be 5 increases detected") {
    assert(DepthDifference.partTwo("firstday.sample.source.path") == 5)
  }

  test("Given test data, there should be 1737 increases detected") {
    assert(DepthDifference.partTwo("firstday.source.path") == 1737)
  }

}