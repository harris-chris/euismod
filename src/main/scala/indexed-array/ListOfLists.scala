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
      def indices(self: Self) = self.indices
      def getElem(self: List1d[I0, T], i: Int) = self.data(i)
      def getNil(self: Self) = List1d[I0, T](Index.empty[I0], Nil: List[T#T])
      def ::(self: Self, other: (I0, T#T)) = List1d[I0, T](self.indices :+ other._1, self.data :+ other._2)
      //def ::(self: Self, other: T#T) = new Self(
        //self.indices,
        //other :: self.data 
      //)
    }
}
