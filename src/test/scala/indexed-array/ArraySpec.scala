package sportarray

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.nscala_time.time.Imports._

import sportdate.SportDate
import sportdate.{IsSportDateInstances, IsSportDateSyntax}

import Skeleton.{PositionsData, IsIdxElemImplicits, DateType}
import Skeleton.IsIdxElemImplicits._
import Skeleton.IsIdxElem
import IndicesObj._

import ListOfListsObj._

class ArraySpec extends AnyFlatSpec with Matchers {
  val datesIndex = DateIndex(
    List(
      SportDate.YMD(2020,8,1), SportDate.YMD(2020,8,2), SportDate.YMD(2020,8,3), SportDate.YMD(2020,8,4)
  ))
  val values1d = List(0.1, 0.2, 0.3, 0.4)

  "Datum" should "store ref and value" in {
    assert(Datum[DateType, PositionsData](SportDate.YMD(2020,8,1), 0.1).ref == SportDate.YMD(2020,8,1))
    assert(Datum[DateType, PositionsData](SportDate.YMD(2020,8,1), 0.1).value == 0.1)
  }
  "Arr1d" should "return correct values using .loc" in {
    val arr1d = Arr1d[DateType, PositionsData](datesIndex, values1d)
    assert(arr1d.loc(datesIndex.vals(2)) == Some(0.3))
  }
}
