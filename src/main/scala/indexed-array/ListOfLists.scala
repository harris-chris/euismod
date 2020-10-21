package sportarray

import ArrayDefs._
import Skeleton._
import IndicesObj._

import shapeless._
import shapeless.ops.hlist._

object ListOfListsObj {

  case class List1d[I, T: IsElement] (
    data: List[T],
  )
  implicit def list1dIsSpArr[I, T: IsElement] = IsArray[List1d[I, T], T](
    getEmpty = self => List1d[I, T](List()),
    getAtN = (self, n) => self.data(n),
    length = self => self.data.length,
    cons = (self, elem) => List1d(elem :: self.data),
  )

  //val l1d = List1d[Char, Double](Index(List('a', 'b', 'c')), List(1.0, 2.0, 3.0))
  //implicitly[IsSpArr[List1d[Char, Double]]]
  //implicitly[IsSpArr[List1d[Char, Int]]]

  //case class List2d[I0, I1, T: DataType] ( 
    //indices: (Index[I0], Index[I1]),
    //data: List[List[T]],
  //)
  //implicit def list2dIsSpArr[A, I0, I1, T: DataType] = 
    //new Is2dSpArr[List2d, I0, I1, T, List1d] {
      //def getIdx(self: Self) = self.indices._1
      //def getElem(self: Self, i: Int): List1d[I1, T] = List1d(self.indices._2, self.data(i))
      //def getNil(self: Self) = List2d[I0, I1, T]((Index.empty[I0], Index.empty[I1]), Nil: List[List[T]])
      //def ::(self: Self, other: (I0, List1d[I1, T])) = List2d[I0, I1, T](
        //(self.indices._1 :+ other._1, self.indices._2), self.data :+ other._2.data
      //)
      //def length(self: Self): Int = self.data.length
      ////def ::(self: Self, other: T#T) = new Self(
        ////self.indices,
        ////other :: self.data 
      ////)
    //}
  //case class List3d[I0, I1, I2, T: DataType] ( 
    //indices: (Index[I0], Index[I1], Index[I2]),
    //data: List[List[List[T]]],
  //)
  //implicit def list3dIsSpArr[A, I0, I1, I2, T: DataType] = 
    //new Is3dSpArr[List3d, I0, I1, I2, T, List1d, List2d] {
      //def getIdx(self: Self) = self.indices._1
      //def getElem(self: Self, i: Int) = List2d((self.indices._2, self.indices._3), self.data(i))
      //def getNil(self: Self) = List3d[I0, I1, I2, T](
        //(Index.empty[I0], Index.empty[I1], Index.empty[I2]), Nil: List[List[List[T]]]
      //)
      //def ::(self: Self, other: (I0, List2d[I1, I2, T])) = List3d[I0, I1, I2, T](
        //(self.indices._1 :+ other._1, self.indices._2, self.indices._3), self.data :+ other._2.data
      //)
      //def length(self: Self): Int = self.data.length
      ////def ::(self: Self, other: T#T) = new Self(
        ////self.indices,
        ////other :: self.data 
      ////)
    //}
}
