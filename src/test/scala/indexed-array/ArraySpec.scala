package sportarray

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ListOfListsObj._

class ArraySpec extends AnyFlatSpec with Matchers {
  "Datum" should "successfully initialize" in {
    assert(Datum(SportDate.YMD(2020,8,1), 0.1).ref == SportDate.YMD(2020,8,1))
    assert(Datum(SportDate.YMD(2020,8,1), 0.1).value == 0.1)
  }
}
