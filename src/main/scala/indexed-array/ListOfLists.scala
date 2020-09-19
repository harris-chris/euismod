package sportarray

import ArrayDefs._
import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsIdxElem}
import IndicesObj._

object ListOfListsObj {

  case class List1d[I0: IsIdxElem, T <: DataType] (
    indices: Index[I0],
    data: List[T#T],
  )
  implicit def list1dIs1dSpArr[A, I0: IsIdxElem, T <: DataType] = 
    new Is1dSpArr[List1d[I0, T], I0, T] {
      //type Self = List1d[N, I0, T]
      def getElem(self: List1d[I0, T], i: Int) = self.data(i)
      //def indices(self: Self) = self.indices
      //def ::(self: Self, other: T#T) = new Self(
        //self.indices,
        //other :: self.data 
      //)
    }
  object List1d {
    //implicit def list1dIs1dSpArr[I0: IsIdxElem, T <: DataType]: Is1dSpArr[List1d[I0, T], I0, T] = 
      //new Is1dSpArr[List1d[I0, T], I0, T] {
        ////type Self = List1d[N, I0, T]
        //def getElem(self: Self, i: Int) = self.data(i)
        ////def indices(self: Self) = self.indices
        ////def ::(self: Self, other: T#T) = new Self(
          ////self.indices,
          ////other :: self.data 
        ////)
      //}
  }

}
