package sportarray

import ArrayDefs._
import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsIdxElem}
import IndicesObj._

import shapeless._
import shapeless.ops.hlist._

object ListOfListsObj {

  case class List1d[T <: DataType, I0: IsIdxElem] (
    indices: Index[I0],
    data: List[T#T],
  )
  implicit def list1dIs1dSpArr[A, T <: DataType, I0: IsIdxElem] = 
    new Is1dSpArr[List1d[T, I0], T, I0] {
      def getIdx(self: Self) = self.indices
      def getElem(self: Self, i: Int) = self.data(i)
      def getNil(self: Self) = List1d[T, I0](Index.empty[I0], Nil: List[T#T])
      def ::(self: Self, other: (I0, T#T)) = List1d[T, I0]((getIdx(self) :+ other._1), self.data :+ other._2)
      def length(self: Self) = self.data.length
    }
  
  case class List2d[T <: DataType, I0: IsIdxElem, I1: IsIdxElem] ( 
    indices: (Index[I0], Index[I1]),
    data: List[List[T#T]],
  )
  implicit def list2dIs2dSpArr[A, T <: DataType, I0: IsIdxElem, I1: IsIdxElem] = 
    new Is2dSpArr[List2d[T, I0, I1], T, I0, I1, List1d] {
      def getIdx(self: Self) = self.indices._1
      def getElem(self: Self, i: Int) = List1d(self.indices._2, self.data(i))
      def getNil(self: Self) = List2d[T, I0, I1]((Index.empty[I0], Index.empty[I1]), Nil: List[List[T#T]])
      def ::(self: Self, other: (I0, List1d[T, I1])) = List2d[T, I0, I1](
        (self.indices._1 :+ other._1, self.indices._2), self.data :+ other._2.data
      )
      def length(self: Self): Int = self.data.length
      //def ::(self: Self, other: T#T) = new Self(
        //self.indices,
        //other :: self.data 
      //)
    }
}
