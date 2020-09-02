package sportarray

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.nscala_time.time.Imports._

import sportdate.SportDate
import sportdate.{IsSportDateInstances, IsSportDateSyntax}

import Skeleton.{IsDate, PositionsData, IsIdxElemImplicits}
import Skeleton.IsIdxElemImplicits._
import Skeleton.IsIdxElem

import ListOfListsObj._

class ArraySpec extends AnyFlatSpec with Matchers {
  "Datum" should "successfully initialize" in {
    assert(Datum[DateTime, PositionsData](SportDate.YMD(2020,8,1), 0.1).ref == SportDate.YMD(2020,8,1))
    //assert(Datum(SportDate.YMD(2020,8,1), 0.1).value == 0.1)
  }
}
