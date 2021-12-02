package second

import org.scalatest.FunSuite

/** Tests around Depth Difference */
class Day2Suite extends FunSuite {

  // PART ONE

  test("Using part one sample input should return 900") {
    assert(CourseCounter.partOne("secondday.sample.source.path") == 900)
  }

  test("Using part one test input should return X") {
    assert(CourseCounter.partOne("secondday.source.path") == 150)
  }


}