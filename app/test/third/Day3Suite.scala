package third

import org.scalatest.FunSuite

/** Tests around Depth Difference */
class Day3Suite extends FunSuite {

  // PART ONE

  test("Using part one sample input should return 198") {
    assert(BinaryCounter.partOne("thirdday.sample.source.path") == 198)
  }

  test("Using part one sample input should return 693486") {
    assert(BinaryCounter.partOne("thirdday.source.path") == 693486)
  }

  test("Using part two sample input should return 230") {
    assert(BinaryCounter.partTwo("thirdday.sample.source.path") == 230)
  }

  test("Using part two input should return 3379326") {
    assert(BinaryCounter.partTwo("thirdday.source.path") == 3379326)
  }

}