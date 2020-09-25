package sportarray

import ArrayDefs._
import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsIdxElem}
import IndicesObj._

import shapeless._
import shapeless.ops.hlist._

object ListOfListsObj {

  case class List1d[T <: DataType, I0: IsIdxElem] (
    indices: Index[I0] :: HNil,
    data: List[T#T],
  )
  implicit def list1dIs1dSpArr[A, T <: DataType, I0: IsIdxElem] = 
    new Is1dSpArr[List1d[T, I0], T, I0] {
      def indices(self: Self): Index[I0] :: HNil = self.indices
      def getElem(self: Self, i: Int) = self.data(i)
      def getNil(self: Self) = List1d[T, I0](Index.empty[I0] :: HNil, Nil: List[T#T])
      //def ::(self: Self, other: (I0, T#T)) = List1d[I0, T](self.indices :+ other._1, self.data :+ other._2)
      //def ::(self: Self, other: T#T) = new Self(
        //self.indices,
        //other :: self.data 
      //)
    }
  
  //case class List2d[I0: IsIdxElem, I1: IsIdxElem, T <: DataType] ( 
    //indices: (Index[I0], Index[I1]),
    //data: List[List[T#T]],
  //)
  //implicit def list2dIs2dSpArr[A, I0: IsIdxElem, I1: IsIdxElem, T <: DataType] = 
    //new Is2dSpArr[List2d[I0, I1, T], I0, I1, T, List1d[I1, T]] {
      //def indices(self: Self) = self.indices
      //def getElem(self: List2d[I0, I1, T], i: Int) = List1d(self.indices._2, self.data(i))
      //def getNil(self: Self) = List2d[I0, I1, T]((Index.empty[I0], Index.empty[I1]), Nil: List[List[T#T]])
      //def ::(self: Self, other: (I0, List1d[I1, T])) = List2d[I0, I1, T](
        //(self.indices._1 :+ other._1, self.indices._2), self.data :+ other._2.data
      //)
      ////def ::(self: Self, other: T#T) = new Self(
        ////self.indices,
        ////other :: self.data 
      ////)
    //}
}
